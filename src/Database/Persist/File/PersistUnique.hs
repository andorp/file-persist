{-# LANGUAGE DeriveDataTypeable #-}
--{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Database.Persist.File.PersistUnique where

import           Data.Aeson as Aeson
import           Data.Char
import           Data.List (groupBy, intersperse)
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as Text
import           Database.Persist.Class
import           Database.Persist.Sql hiding (update, updateField)
import           Database.Persist.TH
import           Web.PathPieces

import           Control.Applicative ((<$>), (<*>))
import           Control.Arrow
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.String
import qualified Data.Text as Text
import           Data.Typeable

import           Database.Persist.File.Directory
import           Database.Persist.File.FileBackend
import           Database.Persist.File.PersistStore

    liftIO $ do
      exist <- doesEntityExist baseDir key
      print $ entityDirFromKey baseDir key
      when exist . removeDirectoryRecursive $ entityDirFromKey baseDir key

  -- Update individual fields on a specific record.
  -- update :: (MonadIO m, PersistEntity val, backend ~ PersistEntityBackend val) => Key val -> [Update val] -> ReaderT backend m ()
  update key updates = do
    FileBackend baseDir <- ask
    let entityDir = entityDirFromKey baseDir key
    forM_ updates $ error "update key updates"
      --updateAlg
      --  updateCons persistFieldDef id id
      --  (const $ return ())
    where
      updateCons x y z = error "update key updates"
    --error "PersistStore FileBackend :: update is undefined"
{-
  -- return meta-data for a given EntityField
  --persistFieldDef :: EntityField record typ -> FieldDef
  persistFieldDef record = undefined
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

update
  updateCons updateField updateValue updateUpdate
  backendUpdate backendSpecificUpdate
  u = case u of
    Update field value update_ -> updateCons (updateField field) (updateValue value) (updateUpdate update_)
    BackendUpdate bsu -> backendUpdate backendSpecificUpdate

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
-}

-- * Persistent Helpers

entityDBName :: (PersistEntity record, PersistEntityBackend record ~ FileBackend)
               => record -> Text
entityDBName = unDBName . entityDB . entityDef . Just

entityDBFields :: (PersistEntity record, PersistEntityBackend record ~ FileBackend)
               => record -> [FieldDef]
entityDBFields = entityFields . entityDef . Just

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

instance FromJSON (BackendKey FileBackend) where
  parseJSON = fmap FileBackendKey . parseJSON

instance ToJSON (BackendKey FileBackend) where
  toJSON = toJSON . key

instance PersistField (BackendKey FileBackend) where
  -- toPersistValue :: a -> PersistValue
  toPersistValue = toPersistValue . key

  -- fromPersistValue :: PersistValue -> Either Text a
  fromPersistValue = either Left (Right . FileBackendKey) . fromPersistValue

instance PathPiece (BackendKey FileBackend) where
  --fromPathPiece :: Text -> Maybe s
  fromPathPiece = error "PathPiece (BackendKey FileBackend) :: fromPathPiece is undefined"
  --toPathPiece :: s -> Text
  toPathPiece = error "PathPiece (BackendKey FileBackend) :: toPathPiece is undefined"

instance PersistFieldSql (BackendKey FileBackend) where
  --sqlType :: Monad m => m a -> SqlType
  sqlType _ = SqlString


instance PersistUnique FileBackend where
  -- Minimal complete definition
  -- getBy, deleteBy

  -- Get a record by unique key, if available. Returns also the identifier.
  -- getBy :: (MonadIO m, backend ~ PersistEntityBackend val, PersistEntity val) => Unique val -> ReaderT backend m (Maybe (Entity val))
  getBy val = error "PersistUnique FileBackend :: getBy is undefined"

  -- Delete a specific record by unique key. Does nothing if no record matches.
  -- deleteBy :: (MonadIO m, PersistEntityBackend val ~ backend, PersistEntity val) => Unique val -> ReaderT backend m ()
  deleteBy val = error "PersistUnique FileBackend :: deleteBy is undefined"

instance PersistQuery FileBackend where
  -- Minimal complete definition
  -- updateWhere, deleteWhere, selectSourceRes, selectKeysRes, count

  -- Update individual fields on any record matching the given criterion.
  -- updateWhere :: (MonadIO m, PersistEntity val, backend ~ PersistEntityBackend val) => [Filter val] -> [Update val] -> ReaderT backend m ()
  updateWhere filters updates = error "PersistQuery FileBackend :: updateWhere is undefined"

  -- Delete all records matching the given criterion.
  -- deleteWhere :: (MonadIO m, PersistEntity val, backend ~ PersistEntityBackend val) => [Filter val] -> ReaderT backend m ()
  deleteWhere filters = error "PersistQuery FileBackend :: deleteWhere is undefined"

  -- Get all records matching the given criterion in the specified order. Returns also the identifiers.
  -- selectSourceRes :: (PersistEntity val, PersistEntityBackend val ~ backend, MonadIO m1, MonadIO m2) => [Filter val] -> [SelectOpt val] -> ReaderT backend m1 (Acquire (Source m2 (Entity val)))
  selectSourceRes filters selects = error "PersistQuery FileBackend :: selectSourceRes is undefined"

  -- Get the Keys of all records matching the given criterion.
  -- selectKeysRes :: (MonadIO m1, MonadIO m2, PersistEntity val, backend ~ PersistEntityBackend val) => [Filter val] -> [SelectOpt val] -> ReaderT backend m1 (Acquire (Source m2 (Key val)))
  selectKeysRes filters selectes = error "PersistQuery FileBackend :: selectKeysRes is undefined"

  -- The total number of records fulfilling the given criterion.
  -- count :: (MonadIO m, PersistEntity val, backend ~ PersistEntityBackend val) => [Filter val] -> ReaderT backend m Int
  count filters = error "PersistQuery FileBackend :: count is undefined"

-- * Example PersistEntityBackend

data ExampleBackendEntity = ExampleBackendEntity {
    entityName :: String
  }

instance PersistEntity ExampleBackendEntity where
  -- Persistent allows multiple different backends (databases)
  type PersistEntityBackend ExampleBackendEntity = FileBackend

  -- By default, a backend will automatically generate the key Instead you can specify a Primary key made up of unique values.
  data Key ExampleBackendEntity = ExampleBackendEntityKey { exKey :: !Text }
    deriving (Eq, Ord, Read, Show)

  -- An EntityField is parameterised by the Haskell record it belongs to and the additional type of that field
  -- data EntityField record :: * -> *
  data EntityField ExampleBackendEntity a = ExampleEntityField

  -- Unique keys besides the Key
  data Unique ExampleBackendEntity = ExampleUnique

  -- a lower-level key operation
  --keyToValues :: Key record -> [PersistValue]
  keyToValues key = undefined

  -- a lower-level key operation
  -- keyFromValues :: [PersistValue] -> Either Text (Key record)
  keyFromValues = undefined

  -- a meta-operation to retrieve the Key EntityField
  -- persistIdField :: EntityField record (Key record)
  persistIdField = undefined

  -- retrieve the EntityDef meta-data for the record
  --entityDef :: Monad m => m record -> EntityDef
  entityDef record = undefined

  -- return meta-data for a given EntityField
  --persistFieldDef :: EntityField record typ -> FieldDef
  persistFieldDef record = undefined

  -- A meta-operation to get the database fields of a record
  --toPersistFields :: record -> [SomePersistField]
  toPersistFields record = undefined

  -- A lower-level operation to convert from database values to a Haskell record
  --fromPersistValues :: [PersistValue] -> Either Text record
  fromPersistValues values = undefined

  -- A meta operation to retrieve all the Unique keys
  --persistUniqueKeys :: record -> [Unique record]
  persistUniqueKeys record = undefined

  -- A lower level operation
  --persistUniqueToFieldNames :: Unique record -> [(HaskellName, DBName)]
  persistUniqueToFieldNames record = undefined

  -- A lower level operation
  --persistUniqueToValues :: Unique record -> [PersistValue]
  persistUniqueToValues record = undefined

  -- Use a PersistField as a lens
  --fieldLens :: EntityField record field -> (forall f. Functor f => (field -> f field) -> Entity record -> f (Entity record))
  fieldLens record = undefined -- lens

type ExampleBackendEntityKey = Key ExampleBackendEntity

instance FromJSON (Key ExampleBackendEntity) where
  parseJSON = fmap ExampleBackendEntityKey . parseJSON

instance ToJSON (Key ExampleBackendEntity) where
  toJSON = toJSON . exKey

instance PersistField (Key ExampleBackendEntity) where
  -- toPersistValue :: a -> PersistValue
  toPersistValue = toPersistValue . exKey

  -- fromPersistValue :: PersistValue -> Either Text a
  fromPersistValue = either Left (Right . ExampleBackendEntityKey) . fromPersistValue

-- * Helpers

-- TODO: Complex keys needs to be handled
keyString key = unPersistText . head $ keyToValues key
  where
    unPersistText (PersistText t) = Text.unpack t
    unPersistText _ = error $ "unPersistText:" ++ show key

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

-- TODO: Rename
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


readField :: FilePath -> String -> IO PersistValue
readField entityPath fieldName = read <$> readFile (entityPath </> fieldName)

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

-- Converts haskell name to lower case persist name
-- HACK to get the same info as the working foreign
-- key would give us
-- EG: BlogPost -> blog_post
haskellNameToDBName
  = map toLower 
  . concat
  . intersperse "_"
  . groupBy (\a b -> isUpper a && isLower b)

-- * Tests

-- Label the given Equivalence classes for a given property
labelEqClasses :: (a -> String) -> (a -> Property) -> (a -> Property)
labelEqClasses labeler prop x = label (labeler x) (prop x)

-- * Named Properties

newtype NamedProperty = NamedProp (String, Property)

named name prop = NamedProp (name, prop)

class PropertyName p where
  getName :: p -> String

instance PropertyName Property where
  getName _ = "Unnamed Property" 

instance PropertyName NamedProperty where
  getName (NamedProp (n,_p)) = n

instance (Show a, PropertyName p) => PropertyName (a -> p) where
  getName f = getName (f (error "PropertyName (a -> p) warning strict evaluation in PropertyName implementation!"))

instance Testable NamedProperty where
  property   (NamedProp (_n,p)) = p
  exhaustive (NamedProp (_n,p)) = exhaustive p

-- * Arbitrary instances

instance Arbitrary (BackendKey FileBackend) where
  arbitrary = fmap (FileBackendKey . fromString) arbitrary
  shrink    = map  (FileBackendKey . fromString) . shrink . Text.unpack . key

instance Arbitrary (Key ExampleBackendEntity) where
  arbitrary = fmap (ExampleBackendEntityKey . fromString) arbitrary
  shrink    = map  (ExampleBackendEntityKey . fromString) . shrink . Text.unpack . exKey

class KeyLength k where
  keyLength :: k -> Int

instance KeyLength FileBackendKey where
  keyLength = Text.length . key

instance KeyLength ExampleBackendEntityKey where
  keyLength = Text.length . exKey

emptyNonEmptyLabel k = case keyLength k of
  0 -> "Empty"
  1 -> "One"
  _ -> "Non Empty"

-- * Test Cases

-- Checks if the From To JSON is an inverse
fromJSONtoJSON_inverse :: (Eq key, Show key, Arbitrary key, FromJSON key, ToJSON key, KeyLength key) => key -> Property
fromJSONtoJSON_inverse = labelEqClasses emptyNonEmptyLabel $ (\k -> (Aeson.Success k) === (fromJSON $ toJSON k))

-- Checks if the From To PersistValue is an inverse
persistValue_inverse :: (Eq key, Show key, Arbitrary key, PersistField key, KeyLength key) => key -> Property
persistValue_inverse = labelEqClasses emptyNonEmptyLabel $ (\k -> (Right k) === (fromPersistValue $ toPersistValue k))

fileBackendKey_fromJSONtoJSON_inverse :: FileBackendKey -> Property
fileBackendKey_fromJSONtoJSON_inverse = fromJSONtoJSON_inverse

fileBackendKey_persistValue_inverse :: FileBackendKey -> Property
fileBackendKey_persistValue_inverse = persistValue_inverse

exampleBackendEntityKey_fromJSONtoJSON_inverse :: ExampleBackendEntityKey -> Property
exampleBackendEntityKey_fromJSONtoJSON_inverse = fromJSONtoJSON_inverse

exampleBackendEntityKey_persistValue_inverse :: ExampleBackendEntityKey -> Property
exampleBackendEntityKey_persistValue_inverse = persistValue_inverse


-- * Collection of all tests

tests = do
  mapM_ namedQuickCheck $ [
      named "Checks if the From To JSON is an inverse (File Backend Key)" $ forAll arbitrary fileBackendKey_fromJSONtoJSON_inverse ,
      named "Checks if the From To PersistValue is an inverse (File Backend Key)" $ forAll arbitrary fileBackendKey_persistValue_inverse ,
      named "Checks if the From To JSON is an inverse (Example Entity Key)" $ forAll arbitrary exampleBackendEntityKey_fromJSONtoJSON_inverse ,
      named "Checks if the From To PersistValue is an inverse (Example Entity Key)" $ forAll arbitrary exampleBackendEntityKey_persistValue_inverse
    ]
  where
    namedQuickCheck np = do
      putStrLn $ getName np
      quickCheck np
