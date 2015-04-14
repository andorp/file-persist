{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Database.Persist.File.FileBackend where

import Prelude hiding (writeFile)

import           Data.Aeson as Aeson
import           Data.Bifunctor
import           Data.Char
import           Data.List (groupBy, intersperse)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as Text
import           Database.Persist.Class
import           Database.Persist.Sql hiding (update, updateField)
import           Database.Persist.TH
import           Web.PathPieces

import           Control.Applicative ((<$>), (<*>))
import           Control.Arrow
import           Control.DeepSeq (deepseq)
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.String
import qualified Data.Text as Text
import           Data.Typeable
import           System.IO hiding (writeFile)

import           Test.QuickCheck

import           Database.Persist.File.Directory

data FileBackend = FileBackend {
    baseDir    :: FilePath
  } deriving (Eq, Show)

runFileBackend :: FileBackend -> ReaderT FileBackend IO a -> IO a
runFileBackend = flip runReaderT

data FileBackendException
  = FBE_General
  | FBE_PersistValueConversion Text
  | FBE_KeyExist Text
  | FBE_KeyDoesNotExist Text
  | FBE_FieldDoesNotExist Text
  deriving (Eq, Typeable, Show)

instance Exception FileBackendException

-- * Persistent Helpers

--type FileBackendEntity record = (PersistEntity record, PersistEntityBackend record ~ FileBackend)

entityDBName :: (PersistEntity record, PersistEntityBackend record ~ FileBackend)
               => record -> Text
entityDBName = unDBName . entityDB . entityDef . Just

entityDBFields :: (PersistEntity record, PersistEntityBackend record ~ FileBackend)
               => record -> [FieldDef]
entityDBFields = entityFields . entityDef . Just

type PersistValueMap = Map FieldDef PersistValue

-- Creates a map from a given entity record, that maps a field def into a PersistValue
persistEntityValueMap :: (PersistEntity record, PersistEntityBackend record ~ FileBackend) => record -> PersistValueMap
persistEntityValueMap val = Map.fromList $ zip <$> entityDBFields <*> (map toPersistValue . toPersistFields) $ val

recordTypeFromKey :: Key record -> record
recordTypeFromKey _ = error "recordTypeFromKey"

keyFromDir :: (PersistEntity record) => FilePath -> Key record
keyFromDir = fromRight . keyFromValues . keyFrom . takeBaseName
  where
    keyFrom t = [PersistText $ fromString t]
    fromRight (Right x) = x

entityDirFromKey :: (PersistEntity record, PersistEntityBackend record ~ FileBackend)
                 => FilePath -> Key record -> FilePath
entityDirFromKey baseDir key = (baseDir </> entityName </> keyStr) where
  keyStr      = keyString key
  entityName  = Text.unpack $ entityDBName $ recordTypeFromKey key

doesEntityExist :: (PersistEntity record, PersistEntityBackend record ~ FileBackend)
                => FilePath -> Key record -> IO Bool
doesEntityExist baseDir key = doesDirectoryExist $ entityDirFromKey baseDir key

insertDBValue :: (PersistEntity record, PersistEntityBackend record ~ FileBackend)
              => (FilePath -> IO FilePath) -> FilePath -> record -> IO FilePath 
insertDBValue createEntityDir baseDir val = do
  let entityName = Text.unpack $ entityDBName val
  let entityDirBase = baseDir </> entityName
  entityDir <- createEntityDir entityDirBase
  let values = zip <$> entityDBFields <*> (map toPersistValue . toPersistFields) $ val
  forM_ values $ \(fieldInfo, persistValue) ->
    saveField baseDir entityDir fieldInfo persistValue
  return entityDir

replaceDBValue :: (PersistEntity record, PersistEntityBackend record ~ FileBackend)
               => FilePath -> Key record -> record -> IO ()
replaceDBValue baseDir key val = do
  let values = zip <$> entityDBFields <*> (map toPersistValue . toPersistFields) $ val
  let entityDir = entityDirFromKey baseDir key
  forM_ values $ \(fieldInfo, persistValue) ->
    updateField baseDir entityDir fieldInfo persistValue

updateField :: FilePath -> FilePath -> FieldDef -> PersistValue -> IO ()
updateField basePath entityPath field value =
  referenceDef
    noReference
    foreignRef (Text.unpack . unHaskellName) id
    embedRef id
    compositeRef id
    (fieldReference field)
  where
    fieldName = Text.unpack . unDBName $ fieldDB field
    writeFieldToDisk = writeFile (entityPath </> fieldName) (show value)

    noReference = writeFieldToDisk
    
    foreignRef haskellName _fieldType = do
      let haskellDBName = haskellNameToDBName haskellName
      onPersistText (return ()) (createSymlink haskellDBName . unPersistText) value
      writeFieldToDisk

    createSymlink haskellName idVal = do
      let keyDir = entityPath </> fieldName <.> "lnk"
      removeDirectoryRecursive keyDir
      createDirectoryIfMissing True keyDir
      createSymbolicLink (".." </> ".." </> ".." </> ".." </> basePath </> haskellName </> idVal) (keyDir </> idVal)

    embedRef _ = return ()
    compositeRef _ = return ()

    unPersistText (PersistText t) = Text.unpack t
    unPersistText _ = error $ "updateField unPersistText:" ++ show [show entityPath, show field, show value]

    onPersistText _ f p@(PersistText _) = f p
    onPersistText x _ _                 = x


readField :: FilePath -> FieldDef -> IO PersistValue
readField entityPath field = do
  let fieldName = Text.unpack . unDBName $ fieldDB field
  let filePath = entityPath </> fieldName
  h <- openFile filePath ReadMode
  s <- hGetContents h
  s `deepseq` hClose h
  return $! read s

writeFile :: FilePath -> String -> IO ()
writeFile path content = do
  handler <- openFile path WriteMode
  hPutStr handler content
  hClose handler

saveField :: FilePath -> FilePath -> FieldDef -> PersistValue -> IO ()
saveField basePath entityPath field value =
  referenceDef
    noReference
    foreignRef (Text.unpack . unHaskellName) id
    embedRef id
    compositeRef id
    (fieldReference field)
  where
    fieldName = Text.unpack . unDBName $ fieldDB field
    embedRef _embedEntityDef = return ()
    compositeRef _compositeDef = return ()
    writeFieldToDisk = writeFile (entityPath </> fieldName) (show value)

    noReference = writeFieldToDisk

    foreignRef haskellName _fieldType = do
      let haskellDBName = haskellNameToDBName haskellName
      onPersistText (return ()) (createSymlink haskellDBName . unPersistText) value
      writeFieldToDisk

    createSymlink haskellName idVal = do
      let keyDir = entityPath </> fieldName <.> "lnk"
      createDirectoryIfMissing True keyDir
      createSymbolicLink (".." </> ".." </> ".." </> ".." </> basePath </> haskellName </> idVal) (keyDir </> idVal)

    unPersistText (PersistText t) = Text.unpack t
    unPersistText _ = error $ "saveField unPersistText:" ++ show [show basePath, show entityPath, show field, show value]

    onPersistText _ f p@(PersistText _) = f p
    onPersistText x _ _                 = x


-- TODO: Complex keys needs to be handled
keyString key = unPersistText . head $ keyToValues key
  where
    unPersistText (PersistText t) = Text.unpack t
    unPersistText _ = error $ "unPersistText:" ++ show key

class RecordContainer (c :: * -> *) where
  getRecord :: c rec -> rec

instance RecordContainer Entity where
  getRecord (Entity _key record) = record

-- * Interfacing the persitent API

{-
-- Persistent users use combinators to create these
data Update record = forall typ. PersistField typ => Update
    { updateField :: EntityField record typ
    , updateValue :: typ
    -- FIXME Replace with expr down the road
    , updateUpdate :: PersistUpdate
    }
    | BackendUpdate
          (BackendSpecificUpdate (PersistEntityBackend record) record)
-}

updateAlg :: (forall typ . PersistField typ => EntityField record typ -> typ -> PersistUpdate -> a)
          -> (b -> a) -> (BackendSpecificUpdate (PersistEntityBackend record) record -> b)
          -> Update record
          -> a
updateAlg
  update
  backendUpdate backendSpecificUpdate
  u = case u of
    Update field value update_ -> update field value update_
    BackendUpdate bsu -> backendUpdate (backendSpecificUpdate bsu)

persistUpdate
  assign
  add
  substract
  multiply
  divide
  p = case p of
    Assign   -> assign
    Add      -> add
    Subtract -> substract
    Multiply -> multiply
    Divide   -> divide

referenceDef
  noReference
  foreignRef   haskellName fieldType
  embedRef     embedEntityDef
  compositeRef compositeDef
  r = case r of
    NoReference      -> noReference
    ForeignRef hn ft -> foreignRef (haskellName hn) (fieldType ft)
    EmbedRef e       -> embedRef (embedEntityDef e)
    CompositeRef c   -> compositeRef (compositeDef c)

{-
-- Persistent users use combinators to create these
data Filter record = forall typ. PersistField typ => Filter
    { filterField  :: EntityField record typ
    , filterValue  :: Either typ [typ] -- FIXME
    , filterFilter :: PersistFilter -- FIXME
    }
    | FilterAnd [Filter record] -- ^ convenient for internal use, not needed for the API
    | FilterOr  [Filter record]
    | BackendFilter
          (BackendSpecificFilter (PersistEntityBackend record) record)
-}

--{-
filterAlg :: (forall typ . PersistField typ => EntityField record typ -> Either typ [typ] -> PersistFilter -> a)
          -> (b -> a) -> ([a] -> b)
          -> (b -> a) -> ([a] -> b)
          -> (c -> a) -> ((BackendSpecificFilter (PersistEntityBackend record) record) -> c)
          -> Filter record
          -> a
filterAlg
  filter
  filterAnd filterAndList
  filterOr  filterOrList
  backendFilter backendSpecificFilter
  f = case f of
    Filter filterField filterValue filterFilter -> filter filterField filterValue filterFilter
    FilterAnd fs    -> filterAnd . filterAndList $ map filterAlg' fs
    FilterOr  fs    -> filterOr  . filterOrList  $ map filterAlg' fs
    BackendFilter b -> backendFilter (backendSpecificFilter b)
  where
    filterAlg' = filterAlg
      filter
      filterAnd filterAndList
      filterOr  filterOrList
      backendFilter backendSpecificFilter
--}

-- Returns True if the given field matches the given value in the given persistValueMap
fieldMatch :: (PersistEntity record, PersistField typ)
           => PersistValueMap -> EntityField record typ -> Either typ [typ] -> PersistFilter -> Bool
fieldMatch persistMap field value filter = maybe False id $ do
  let fieldDef = persistFieldDef field
  let value' = bimap toPersistValue (map toPersistValue) value
  entityFieldValue <- Map.lookup fieldDef persistMap
  return $ matcher value' entityFieldValue
  where
    matcher (Right xs) v = persistFilter
      (error $ concat [show xs, " == ", show v])
      (error $ concat [show xs, " /= ", show v])
      (error $ concat [show xs, " > ", show v])
      (error $ concat [show xs, " < ", show v])
      (error $ concat [show xs, " >= ", show v])
      (error $ concat [show xs, " <= ", show v])
      (elem v xs)       -- in_
      (not $ elem v xs) -- notIn
      (\bs -> error $ concat ["Backend specific filter", Text.unpack bs])
      filter
    matcher (Left x) v = persistFilter
      (x == v) -- eq
      (x /= v) -- ne
      (x > v) -- gt
      (x < v) -- lt
      (x >= v) -- ge
      (x <= v) -- le
      (error $ concat [show v, " in ", show x]) -- in_
      (error $ concat [show v, " not in ", show x]) -- notIn
      (\bs -> error $ concat ["Backend specific filter", Text.unpack bs])
      filter

-- Matches the given filter with a given record, if the the record mathces with the filter it returns True,
-- otherwise False.
matches :: (PersistEntity record, PersistEntityBackend record ~ FileBackend) => Filter record -> record -> Bool
matches filter record = matcher (persistEntityValueMap record) filter
  where
    matcher :: (PersistEntity record, PersistEntityBackend record ~ FileBackend)
            => PersistValueMap -> Filter record -> Bool
    matcher recordMap = filterAlg
      (fieldMatch recordMap)
      id and
      id or
      (error ("backend specific filter")) id

{-
data PersistFilter
  = Eq
  | Ne
  | Gt
  | Lt
  | Ge
  | Le
  | In
  | NotIn
  | BackendSpecificFilter Text
-}

persistFilter
  eq
  ne
  gt
  lt
  ge
  le
  in_
  notIn
  backendSpecificFilter
  f = case f of
    Eq -> eq
    Ne -> ne
    Gt -> gt
    Lt -> lt
    Ge -> ge
    Le -> le
    In -> in_
    NotIn -> notIn
    BackendSpecificFilter text -> backendSpecificFilter text

-- Converts haskell name to lower case persist name
-- HACK to get the same info as the working foreign
-- key would give us
-- EG: BlogPost -> blog_post
haskellNameToDBName
  = map toLower 
  . concat
  . intersperse "_"
  . groupBy (\a b -> isUpper a && isLower b)

{-
-- Persistent users use these directly
data SelectOpt record = forall typ. Asc  (EntityField record typ)
                      | forall typ. Desc (EntityField record typ)
                      | OffsetBy Int
                      | LimitTo Int
-}

selectOptAlg
  :: (forall typ . EntityField record typ -> a)
  -> (forall typ . EntityField record typ -> a)
  -> (Int -> a)
  -> (Int -> a)
  -> SelectOpt record
  -> a
selectOptAlg
  asc
  desc
  offsetBy
  limitTo
  s = case s of
    Asc  field -> asc  field
    Desc field -> desc field
    OffsetBy o -> offsetBy o
    LimitTo  o -> limitTo  o

pair a b = (a, b)
