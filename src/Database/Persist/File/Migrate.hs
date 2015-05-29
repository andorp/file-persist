module Database.Persist.File.Migrate where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import qualified Data.Text as Text

import           Database.Persist.File.Base
import           Database.Persist.File.Directory
import           Database.Persist.File.FileBackend
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
  let dirsToCreate = map (baseDir </>) $ entityDefToUniqueRelPaths val
  liftIO $ mapM_ (createDirectoryIfMissing True) dirsToCreate

{-
uniqueDefAlg
  :: (HaskellName -> DBName -> [(HaskellName, DBName)] -> [Attr] -> a)
  -> UniqueDef
  -> a
-}

dirToUniqueRecord eDef urDef =
  let ef = getDBName $ entityDB eDef
      uf = uniqueDefToRelativePaths urDef
  in ef </> uf

uniqueDefToRelativePaths = uniqueDef $ \_haskellName dbname fieldNames _attrs ->
  getDBName dbname
--  let dbName' = getDBName dbname
--  in  dbName' -- :map ((dbName' </>) . getDBName . snd) fieldNames

entityDefToUniqueRelPaths e =
  let entityRelPath = getDBName $ entityDB e
      uniqueRelPaths = (entityRelPath:) . map (entityRelPath </>) . map uniqueDefToRelativePaths $ entityUniques e
  in uniqueRelPaths
