{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Database.Persist.File.FileBackend where

import Prelude hiding (filter, writeFile)

import           Data.Aeson as Aeson
import           Data.Bifunctor
import           Data.Char
import           Data.Hashable
import           Data.List (groupBy, intersperse)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
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

import           Database.Persist.File.Base hiding (updateField)
import           Database.Persist.File.Directory
import           Database.Persist.File.Unique

data FileBackend = FileBackend {
    baseDir :: FilePath
  } deriving (Eq, Show)


type PersistValueMap = Map FieldDef PersistValue

dataDirName     = "data"
metaDataDirName = "meta"

dataDir :: FileBackend -> FilePath
dataDir (FileBackend baseDir) = baseDir </> dataDirName

metaDataDir :: FileBackend -> FilePath
metaDataDir (FileBackend baseDir) = baseDir </> metaDataDirName

runFileBackend :: FileBackend -> ReaderT FileBackend IO a -> IO a
runFileBackend = flip runReaderT

getDataDir :: Monad m => ReaderT FileBackend m FilePath
getDataDir = asks dataDir

getMetaDataDir :: Monad m => ReaderT FileBackend m FilePath
getMetaDataDir = asks metaDataDir

data FileBackendException
  = FBE_General
  | FBE_PersistValueConversion Text
  | FBE_KeyExist Text
  | FBE_KeyDoesNotExist Text
  | FBE_FieldDoesNotExist Text
  | FBE_UniqueContraint Text
  | FBE_AmbigiousUniqueContraintMapping Text
  | FBE_UnkownLinkFile Text
  | FBE_Exception Text SomeException
  deriving (Typeable, Show)

instance Exception FileBackendException

-- * Persistent Helpers

--type FileBackendEntity record = (PersistEntity record, PersistEntityBackend record ~ FileBackend)

entityDBName :: (PersistEntity record, PersistEntityBackend record ~ FileBackend)
               => record -> Text
entityDBName = unDBName . entityDB . entityDef . Just

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

getDBValue :: (PersistEntity record, FileBackend ~ PersistEntityBackend record)
          => record -> FilePath -> IO record
getDBValue recordType entityDir = do
  let fields = entityDBFields recordType
  errorOrValue <- fmap fromPersistValues $ forM fields $ readField entityDir
  return $ case errorOrValue of
    Left err -> throw $ FBE_PersistValueConversion err
    Right v  -> v

insertDBValue :: (PersistEntity record, PersistEntityBackend record ~ FileBackend)
              => (FilePath -> IO FilePath) -> FilePath -> FilePath -> record -> IO FilePath 
insertDBValue createEntityDir baseDir metaDir val = do
  let entityName = Text.unpack $ entityDBName val
  let entityDirBase = baseDir </> entityName
  entityDir <- createEntityDir entityDirBase
  let values = zip <$> entityDBFields <*> (map toPersistValue . toPersistFields) $ val
  forM_ values $ \(fieldInfo, persistValue) ->
    saveField baseDir entityDir fieldInfo persistValue
  linkUniqueValuesToEntity metaDir entityDir val
  return entityDir

replaceDBValue :: (PersistEntity record, PersistEntityBackend record ~ FileBackend)
               => FilePath -> FilePath -> Key record -> record -> IO ()
replaceDBValue baseDir metaDir key val = do
  let values = zip <$> entityDBFields <*> (map toPersistValue . toPersistFields) $ val
  let entityDir = entityDirFromKey baseDir key
  oldValue <- getDBValue val entityDir
  removeUniqueValueLink metaDir oldValue
  forM_ values $ \(fieldInfo, persistValue) ->
    updateField baseDir entityDir fieldInfo persistValue
  linkUniqueValuesToEntity metaDir entityDir val

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
      createSymbolicLink (ancestor 5 (basePath </> haskellName </> idVal)) (keyDir </> idVal)

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
      createSymbolicLink (ancestor 5 (basePath </> haskellName </> idVal)) (keyDir </> idVal)

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
matches filter_ record = matcher (persistEntityValueMap record) filter_
  where
    matcher :: (PersistEntity record, PersistEntityBackend record ~ FileBackend)
            => PersistValueMap -> Filter record -> Bool
    matcher recordMap = filter
      (fieldMatch recordMap)
      id and
      id or
      (error ("backend specific filter")) id

-- Creates a map from a given entity record, that maps a field def into a PersistValue
persistEntityValueMap :: (PersistEntity record, PersistEntityBackend record ~ FileBackend) => record -> PersistValueMap
persistEntityValueMap val = Map.fromList $ zip <$> entityDBFields <*> (map toPersistValue . toPersistFields) $ val

entityDBFields :: (PersistEntity record, PersistEntityBackend record ~ FileBackend)
               => record -> [FieldDef]
entityDBFields = entityFields . entityDef . Just

-- Converts haskell name to lower case persist name
-- HACK to get the same info as the working foreign
-- key would give us
-- EG: BlogPost -> blog_post
haskellNameToDBName
  = map toLower 
  . concat
  . intersperse "_"
  . groupBy (\a b -> isUpper a && isLower b)

-- * Unique

-- Creates a hash value from the given fields which represents a
-- a unique contraint in the database
hashUniqueValue
  :: (PersistEntity record, PersistEntityBackend record ~ FileBackend)
  => Unique record -> UniqueHashPair
hashUniqueValue = hashPair . persistUniqueToValues

-- Extracts the information from an entity and its unique record subset
-- If the UniqueDef part is Nothing, the unique record is not corresponts
-- to the given record.
uniqueRecordToUniqueDef
  :: (PersistEntity record, PersistEntityBackend record ~ FileBackend)
  => record -> Unique record -> (EntityDef, Maybe UniqueDef)
uniqueRecordToUniqueDef record urecord = (ent, Map.lookup key entUniqueMap)
  where
    ent = entityDef (Just record)
    key = persistUniqueToFieldNames urecord
    entUniqueMap = Map.fromList . map (uniqueFields &&& id) . entityUniques $ ent

getHashPathForUniqueKey
  :: (PersistEntity val, PersistEntityBackend val ~ FileBackend)
  => val -> Unique val -> IO UniqueHashPath
getHashPathForUniqueKey value unique = do
  let (entDef, mUniqueDef) = uniqueRecordToUniqueDef value unique
  let en = getDBName $ entityDB entDef
  when (isNothing mUniqueDef) .
    throw . FBE_AmbigiousUniqueContraintMapping $ fromString en
  let un = uniqueDefToDBName $ fromJust mUniqueDef
  return ((en </> un) </>> hashUniqueValue unique)

getFullHashPathForUniqueKey
  :: (PersistEntity val, PersistEntityBackend val ~ FileBackend)
  => FilePath -> val -> Unique val -> IO UniqueHashPath
getFullHashPathForUniqueKey baseMetaDir record uniqueKey =
  (baseMetaDir </>) <$> getHashPathForUniqueKey record uniqueKey

getEntityByUniqueKey
  :: (PersistEntity val, PersistEntityBackend val ~ FileBackend)
  => FilePath -> Unique val -> IO (Maybe (Entity val))
getEntityByUniqueKey baseMetaDir unique = do
  linkPath <- getFullHashPathForUniqueKey baseMetaDir fake unique
  exist <- doesDirectoryExist linkPath
  case exist of
    False -> return Nothing
    True -> do
      realPath <- removeAncestors <$> readSymbolicLink linkPath
      value <- getDBValue fake realPath
      return (Just (Entity (keyFromDir realPath) value))
  where
    fake :: val
    fake = error "getEntityByUniqueKey: fake value is used"
    removeAncestors = joinPath . drop 5 . splitPath -- TODO: Magic number

-- Creates links in the baseMetaDir to all unique values for the given entity
linkUniqueValuesToEntity
  :: (PersistEntity record, PersistEntityBackend record ~ FileBackend)
  => FilePath -> FilePath -> record -> IO ()
linkUniqueValuesToEntity baseMetaDir entityDir record =
  forM_ (persistUniqueKeys record) $ \uniqueKey -> do
    path <- getFullHashPathForUniqueKey baseMetaDir record uniqueKey
    link path
  where
    link uniqueHashPath = do
      exist <- doesDirectoryExist uniqueHashPath
      when exist .
        throw . FBE_UniqueContraint . Text.pack $ concat ["Existing entity: ", uniqueHashPath]
      createDirectoryIfMissing True (uniqueHashPathDir uniqueHashPath)
      do result <- try' $ createSymbolicLink (ancestor 5 entityDir) uniqueHashPath
         case result of
           Left e -> throwIO $ FBE_Exception "CreateLink" e
           Right _ -> return ()

removeUniqueValueLink'
  :: (PersistEntity record, PersistEntityBackend record ~ FileBackend)
  => FilePath -> Unique record -> IO ()
removeUniqueValueLink' baseMetaDir uniqueKey = do
  let record = fake uniqueKey
  forM_ (persistUniqueKeys record) $ \uniqueKey -> do
    path <- getFullHashPathForUniqueKey baseMetaDir record uniqueKey
    removeUniqueLink path
  where
    fake :: Unique record -> record
    fake = error "fake object is used in removeUniqueValueLink"

-- Removes the links from the baseMetaDir from all unique values for the given entity
removeUniqueValueLink
  :: (PersistEntity record, PersistEntityBackend record ~ FileBackend)
  => FilePath -> record -> IO ()
removeUniqueValueLink baseMetaDir record =
  forM_ (persistUniqueKeys record) $ \uniqueKey -> do
    path <- getFullHashPathForUniqueKey baseMetaDir record uniqueKey
    removeUniqueLink path

removeUniqueLink uniqueHashPath = do
  result <- try' $ removeLink uniqueHashPath
  case result of
    Left e -> throwIO $ FBE_Exception "RemoveLink" e
    Right _ -> return ()

deleteRecord baseDir metaDir key value = do
  removeUniqueValueLink metaDir value
  exist <- doesEntityExist baseDir key
  when exist $ do
    removeDirectoryRecursive $ entityDirFromKey baseDir key

deleteByUnique
  :: (PersistEntity record, PersistEntityBackend record ~ FileBackend)
  => FilePath -> FilePath -> Unique record -> IO ()
deleteByUnique baseDir metaDir uniqueKey = do
  entity <- getEntityByUniqueKey baseDir uniqueKey
  onJust entity $ \e ->
    deleteRecord baseDir metaDir (entityKey e) (entityVal e)

-- * Helpers

-- Create the nth ancestor path of the given filepath
ancestor :: Int -> FilePath -> FilePath
ancestor n path = joinPath (replicate n "..") </> path

-- Supposing there is at least n of ".." at the beginning
-- of the path we drop them.
dropAncestorParts :: Int -> FilePath -> FilePath
dropAncestorParts n = joinPath . drop n . splitPath

try' :: IO a -> IO (Either SomeException a)
try' = try

onJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
onJust x k = maybe (return ()) k x
