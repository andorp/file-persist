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
  k0 <- insert (Person "user" (Just 10))
  k1 <- insert (BlogPost "vlog" k0 Nothing)
  k2 <- insert (BlogPost "vlog" k0 (Just k1))
  k3 <- insert (BlogPost "vlog" k0 Nothing)
  print' "insert"
  replace k2 (BlogPost "vlog" k0 (Just k1))
  print' "replace"
  update k1 [BlogPostTitle =. "vlog1"]
  print' "update"
  (get k0) >>= print'
  (get k1) >>= print'
  (get k2) >>= print'
  (get k3) >>= print'
  (count [BlogPostPrevious ==. Nothing]) >>= print'
  delete k2
  return ()
  where

print' :: (MonadIO m, Show a) => a -> m ()
print' = liftIO . print
