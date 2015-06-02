{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Database.Persist.File.PersistStore where

import           Prelude

import           Data.Aeson as Aeson
import           Data.Acquire
import qualified Data.Conduit as Conduit
import qualified Data.Conduit.List as Conduit
import           Database.Persist.Class
import           Database.Persist.Sql hiding (update, updateField)
import           Database.Persist.TH
import           Web.PathPieces

import           Control.Arrow
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Char
import           Data.List (groupBy, intersperse)
import           Data.Maybe (catMaybes)
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Typeable

import           Test.QuickCheck

import           Database.Persist.File.Base as FilePersist hiding (updateField)
import           Database.Persist.File.Directory
import           Database.Persist.File.FileBackend
import qualified Database.Persist.File.PVArithmetics as PV
import           Database.Persist.File.SelectOpts
import           Database.Persist.File.Unique

import Data.Maybe

infixl 4 <$>

-- Monadic fmap, as ReaderT is not a Functor
(<$>) :: Monad m => (a -> b) -> m a -> m b
(<$>) f m = m >>= (return . f)

(<*>) :: Monad m => m (a -> b) -> m a -> m b
(<*>) f m = f >>= (<$> m)

getEntityBaseDir :: (MonadIO m, FileBackend ~ PersistEntityBackend val, PersistEntity val)
                 => val -> ReaderT FileBackend m FilePath
getEntityBaseDir recordType = do
  baseDir <- getDataDir
  let entityName = Text.unpack $ entityDBName recordType
  return $ baseDir </> entityName

getEntityDirs ::  (MonadIO m, FileBackend ~ PersistEntityBackend val, PersistEntity val)
                 => val -> ReaderT FileBackend m [FilePath]
getEntityDirs recordType = do
  entityDir <- getEntityBaseDir recordType
  liftIO $ entryDirs entityDir <$> getDirectoryContents entityDir

-- Return a list of filepath parts that are supposed to be entries for some entities
entryDirs entityBaseDir = map (entityBaseDir </>) . Prelude.filter (not . flip elem [".", ".."])

-- Loads the given type of entity from the given directory.
-- WARINING: The first parameter recordType just a type indicator, it evaluates to botton.
getEntity :: (MonadIO m, FileBackend ~ PersistEntityBackend val, PersistEntity val)
          => val -> FilePath -> ReaderT FileBackend m val
getEntity recordType entityDir = liftIO $ getDBValue recordType entityDir

updateEntity :: (MonadIO m, FileBackend ~ PersistEntityBackend val, PersistEntity val)
             => [Update val] -> FilePath -> ReaderT FileBackend m ()
updateEntity updates entityDir = do
  baseDir <- getDataDir
  forM_ updates $
    updateAlg
      (updateCons baseDir entityDir)
      backendSpecificUpdate id
  where
    backendSpecificUpdate bsu = error "PersistStore FileBackend: backend specific update is unimplemented"

    updateCons baseDir entityDir field b update = liftIO $ do
      let fieldDef = persistFieldDef field
      a <- readField entityDir fieldDef
      let value' = persistUpdate (\_x y -> y) PV.add PV.sub PV.mul PV.div update a (toPersistValue b)
      updateField baseDir entityDir (persistFieldDef field) value'

newtype EntityPath val = EntityPath (FilePath, val)

entityPathAlg f (EntityPath (path, val)) = f path val

instance RecordContainer EntityPath where
  getRecord (EntityPath (_path, record)) = record

getEntityPath :: EntityPath val -> FilePath
getEntityPath (EntityPath (fp, _e)) = fp

filterEntities :: (MonadIO m, FileBackend ~ PersistEntityBackend val, PersistEntity val)
               => [Filter val] -> ReaderT FileBackend m [EntityPath val]
filterEntities filters = do
  let recordType = recordTypeFromFilterList filters
  entityDirs <- getEntityDirs recordType
  mes <- forM entityDirs $ \entityDir -> do
    ent <- matchFilters <$> getEntity recordType entityDir
    return $ entityPath entityDir <$> ent
  return $ catMaybes mes
  where
    entityPath a b = EntityPath (a, b)
    matchFilters entity =
      if and $ map (flip matches entity) filters
        then Just entity
        else Nothing

selectEntityList :: (PersistEntity val, MonadIO m, PersistEntityBackend val ~ FileBackend)
                 => [Filter val] -> [SelectOpt val] -> ReaderT FileBackend m [Entity val]
selectEntityList filters selectOpts = do
  entities <- filterEntities filters
  return . map ((uncurry Entity ^<< keyFromDir *** id) . (entityPathAlg pair))
           $ selects selectOpts entities

instance PersistStore FileBackend where

  -- Minimal complete definition
  -- get, insert, insertKey, repsert, replace, delete, update

  data BackendKey FileBackend = FileBackendKey { key :: !Text }
    deriving (Eq, Ord, Read, Show)

  -- Get a record by identifier, if available.
  -- get :: (MonadIO m, backend ~ PersistEntityBackend val, PersistEntity val) => Key val -> ReaderT backend m (Maybe val)
  get key = do
    let recordType = recordTypeFromKey key
    let keyStr = keyString key -- TODO: Complex keys needs to be handled
    baseDir <- getEntityBaseDir recordType
    let entityDir = baseDir </> keyStr
    exists <- liftIO $ doesDirectoryExist entityDir
    case exists of
      False -> return Nothing
      True  -> (getEntity recordType entityDir) >>= (return . Just)

  -- Create a new record in the database, returning an automatically created key (in SQL an auto-increment id).
  -- insert :: (MonadIO m, backend ~ PersistEntityBackend val, PersistEntity val) => val -> ReaderT backend m (Key val)
  insert val = do
    baseDir <- getDataDir
    metaDir <- getMetaDataDir
    key <- liftIO $ insertDBValue (flip createTempDirectory "") baseDir metaDir val
    return $ keyFromDir key

  -- Create a new record in the database using the given key.
  -- insertKey :: (MonadIO m, backend ~ PersistEntityBackend val, PersistEntity val) => Key val -> val -> ReaderT backend m ()
  insertKey key val = do
    baseDir <- getDataDir
    metaDir <- getMetaDataDir
    let createDir dir = do
          let keyStr = keyString key
          let entityDir = dir </> keyStr
          exist <- doesDirectoryExist entityDir
          when exist $ throw . FBE_KeyExist $ Text.pack keyStr
          createDirectory entityDir
          return entityDir
    liftIO $ insertDBValue createDir baseDir metaDir val
    return ()

  -- Put the record in the database with the given key. Unlike replace, if a record with the given key does not exist then a new record will be inserted.
  -- repsert :: (MonadIO m, backend ~ PersistEntityBackend val, PersistEntity val) => Key val -> val -> ReaderT backend m ()
  repsert key val = do
    baseDir <- getDataDir
    exist <- liftIO $ doesEntityExist baseDir key
    (if exist then replace else insertKey) key val

  -- Replace the record in the database with the given key. Note that the result is undefined if such record does not exist, so you must use 'insertKey or repsert in these cases.
  -- replace :: (MonadIO m, backend ~ PersistEntityBackend val, PersistEntity val) => Key val -> val -> ReaderT backend m ()
  replace key val = do
    baseDir <- getDataDir
    metaDir <- getMetaDataDir
    liftIO $ replaceDBValue baseDir metaDir key val
    --error "PersistStore FileBackend :: replace is undefined"

  -- Delete a specific record by identifier. Does nothing if record does not exist.
  -- delete :: (MonadIO m, backend ~ PersistEntityBackend val, PersistEntity val) => Key val -> ReaderT backend m ()
  delete key = do
    baseDir <- getDataDir
    metaDir <- getMetaDataDir
    value <- get key
    liftIO $ do
      onJust (removeUniqueValueLink metaDir) value
      exist <- doesEntityExist baseDir key
      when exist $ do
        removeDirectoryRecursive $ entityDirFromKey baseDir key

  -- Update individual fields on a specific record.
  -- update :: (MonadIO m, PersistEntity val, backend ~ PersistEntityBackend val) => Key val -> [Update val] -> ReaderT backend m ()
  update key updates = do
    baseDir <- getDataDir
    let entityDir = entityDirFromKey baseDir key
    updateEntity updates entityDir

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
  updateWhere filters updates = do
    entityPaths <- map getEntityPath <$> filterEntities filters
    mapM_ (updateEntity updates) entityPaths

  -- Delete all records matching the given criterion.
  -- deleteWhere :: (MonadIO m, PersistEntity val, backend ~ PersistEntityBackend val) => [Filter val] -> ReaderT backend m ()
  deleteWhere filters = do
    entityPaths <- map getEntityPath <$> filterEntities filters
    liftIO $ mapM_ removeDirectoryRecursive entityPaths

  -- Get all records matching the given criterion in the specified order. Returns also the identifiers.
  -- selectSourceRes :: (PersistEntity val, PersistEntityBackend val ~ backend, MonadIO m1, MonadIO m2)
  --                 => [Filter val] -> [SelectOpt val] -> ReaderT backend m1 (Acquire (Source m2 (Entity val)))
  selectSourceRes filters selectOpts = do
    selectedEntitites <- selectEntityList filters selectOpts
    return $ mkAcquire (return (Conduit.sourceList selectedEntitites)) (const $ return ())

  -- Get the Keys of all records matching the given criterion.
  -- selectKeysRes :: (MonadIO m1, MonadIO m2, PersistEntity val, backend ~ PersistEntityBackend val)
  --               => [Filter val] -> [SelectOpt val] -> ReaderT backend m1 (Acquire (Source m2 (Key val)))
  selectKeysRes filters selectOpts = do
    selectedEntitites <- map entityKey <$> selectEntityList filters selectOpts
    return $ mkAcquire (return (Conduit.sourceList selectedEntitites)) (const $ return ())

  -- The total number of records fulfilling the given criterion.
  -- count :: (MonadIO m, PersistEntity val, backend ~ PersistEntityBackend val) => [Filter val] -> ReaderT backend m Int
  count filters =
    length <$> filterEntities filters


recordTypeFromFilterList :: [Filter record] -> record
recordTypeFromFilterList _ = error "recordTypeFromFilterList"

-- * Backend Key instances

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

-- * Helpers

onJust :: (Monad m) => (a -> m ()) -> Maybe a -> m ()
onJust k = maybe (return ()) k
