{-# LANGUAGE DeriveFunctor #-}
module Database.Persist.File.Unique where

import Data.Hashable
import Data.Ratio
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Fixed

import Database.Persist.File.Base
import Database.Persist.File.Directory

-- Represents an entity with two information to avoid hash
data UniqueHashPair a = UniqueHashPair {
    first  :: a
  , second :: a
  } deriving (Eq, Functor, Show)

infixl 9 </>>

(</>>) :: FilePath -> UniqueHashPair FilePath -> UniqueHashPair FilePath
(</>>) fp (UniqueHashPair p1 p2) = UniqueHashPair (fp </> p1) (fp </> p2)

infix 9 <</>>

(<</>>) :: UniqueHashPair FilePath -> UniqueHashPair FilePath -> UniqueHashPair FilePath
(<</>>) (UniqueHashPair p1 q1) (UniqueHashPair p2 q2) = UniqueHashPair (p1 </> p2) (q1 </> q2)

salt :: UniqueHashPair FilePath -> UniqueHashPair FilePath
salt = uniqueHashPair $ \p1 p2 ->
         UniqueHashPair (saltPath saltOne p1) (saltPath saltTwo p2)
  where-- Join the salt to the end of the path
    saltPath :: Salt -> FilePath -> FilePath
    saltPath salt = (++ (show salt))

uniqueHashPair f (UniqueHashPair p1 p2) = f p1 p2

-- Returns the dbName for the given unique path
uniqueDefToDBName = uniqueDef $ \_haskellName dbname fieldNames _attrs ->
  let name = getDBName dbname
  in salt $ UniqueHashPair name name

dirToUniqueRecord eDef urDef =
  let ef = getDBName $ entityDB eDef
      uf = uniqueDefToDBName urDef
  in ef </>> uf

entityDefToUniqueRelPaths e =
  let entityDBName = getDBName $ entityDB e
      uniqueRelPaths = (entityDBName:) . concatMap (paths . (entityDBName </>>)) . map uniqueDefToDBName $ entityUniques e
  in uniqueRelPaths
  where
    paths = uniqueHashPair (\x y -> [x,y])

type Salt = Int

saltOne, saltTwo :: Salt
saltOne = 0
saltTwo = 1

-- Create a hash pair from the persist value
hashPair :: (Hashable h) => h -> UniqueHashPair Int
hashPair p = UniqueHashPair (hashWithSalt saltOne p) (hashWithSalt saltTwo p)

instance Hashable PersistValue where
  hash = hashWithSalt saltOne
  hashWithSalt = hashPersistValue

-- Converts a PersistValue and a given salt to a hash value value
hashPersistValue s = persistValueAlg
  persistText
  persistByteString
  persistInt64
  persistDouble
  persistRational
  persistBool
  persistDay
  persistTimeOfDay
  persistUTCTime
  persistNull
  persistList
  persistMap
  persistObjectId
  persistDbSpecific
  where
    persistText = hashWithSalt s
    persistByteString = hashWithSalt s
    persistInt64 = hashWithSalt s
    persistDouble = hashWithSalt s
    persistRational r = hashWithSalt s (numerator r, denominator r)
    persistBool = hashWithSalt s
    persistDay = hashWithSalt s . toModifiedJulianDay
    persistTimeOfDay t = hashWithSalt s (todHour t, todMin t, resolution $ todSec t)
    persistUTCTime t = hashWithSalt s (persistDay $ utctDay t, fromEnum $ utctDayTime t)
    persistNull = hashWithSalt s ()
    persistList = hashWithSalt s
    persistMap = hashWithSalt s
    persistObjectId = hashWithSalt s
    persistDbSpecific = hashWithSalt s

