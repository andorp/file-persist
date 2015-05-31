module Database.Persist.File.Migrate where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import qualified Data.Text as Text

import           Database.Persist.File.Base
import           Database.Persist.File.Directory
import           Database.Persist.File.FileBackend
import           Database.Persist.File.Unique
import           Database.Persist.Sql hiding (Migration, migrate)


type Migration = ReaderT FileBackend IO ()

migrate :: [EntityDef] -> EntityDef -> Migration
migrate allDefs val = do
  createEntityDir     allDefs val
  createMetaEntityDir allDefs val

-- Creates a data directory for a given entity
createEntityDir :: [EntityDef] -> EntityDef -> Migration
createEntityDir allDefs val = do
  baseDir <- getDataDir
  let dirName = Text.unpack . unDBName $ entityDB val
  liftIO $ createDirectoryIfMissing True (baseDir </> dirName)

-- Creates a meta data directory for a given entity
createMetaEntityDir :: [EntityDef] -> EntityDef -> Migration
createMetaEntityDir allDefs val = do
  baseDir <- getMetaDataDir
  let uniquePaths = entityDefToUniqueRelPaths val
  let dirsToCreate = map (baseDir </>) $ uniquePaths
  liftIO $ mapM_ (createDirectoryIfMissing True) dirsToCreate
