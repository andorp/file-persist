module Database.Persist.File.Migrate where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import qualified Data.Text as Text

import           Database.Persist.File.Directory
import           Database.Persist.File.FileBackend
import           Database.Persist.Sql hiding (Migration, migrate)


type Migration = ReaderT FileBackend IO ()

migrate :: [EntityDef] -> EntityDef -> Migration
migrate allDefs val = do
  FileBackend baseDir <- ask
  let dirName = Text.unpack . unDBName $ entityDB val
  liftIO $ createDirectoryIfMissing True (baseDir </> dirName)

