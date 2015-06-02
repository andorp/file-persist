{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Entity where

import           Database.Persist
import           Database.Persist.Class
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Language.Haskell.TH.Syntax

import           Database.Persist.File
import           Database.Persist.File.TH

import           Control.Monad.IO.Class

--share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
--share [mkPersist (mkPersistSettings (ConT ''SqlBackend)), mkMigrate "migrateAll"] [persistLowerCase|
share [mkPersist (mkPersistSettings (ConT ''FileBackend)), mkFileMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age  Int Maybe
    deriving Show
BlogPost
    title    String
    authorId PersonId
    previous BlogPostId Maybe
    UniqueTitle title
    UniqueAuthorId authorId
    deriving Show
|]

testFileBackend = runFileBackend (FileBackend "data/") $ do
  migrateAll
  p0 <- insert (Person "user" (Just 10))
  p1 <- insert (Person "user" (Just 10))
  p2 <- insert (Person "user" (Just 10))
  p3 <- insert (Person "user" (Just 10))
  k1 <- insert (BlogPost "vlog0" p0 Nothing)
  k2 <- insert (BlogPost "vlog1" p1 (Just k1))
  k3 <- insert (BlogPost "vlog2" p2 (Just k2))
  printIO "insert"
  replace k2 (BlogPost "vlog4" p3 (Just k3))
  printIO "replace"
  update k1 [BlogPostTitle =. "vlog3"]
  printIO "update"
  (get p0) >>= printIO
  (get k1) >>= printIO
  (get k2) >>= printIO
  (get k3) >>= printIO
  (getBy (UniqueTitle "vlog0")) >>= printIO
  (count [BlogPostPrevious ==. Nothing]) >>= printIO
  delete k2
  return ()
  where

printIO :: (MonadIO m, Show a) => a -> m ()
printIO = liftIO . print
