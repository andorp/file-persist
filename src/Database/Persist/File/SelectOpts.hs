{-# LANGUAGE TypeFamilies #-}
module Database.Persist.File.SelectOpts (
    selects
  ) where

import           Prelude hiding (lookup)

import           Control.Applicative
import           Control.Exception
import           Data.Function (on)
import           Data.List (sortBy, groupBy)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Text as Text

import           Database.Persist.Class
import           Database.Persist.Sql

import           Database.Persist.File.FileBackend


-- Implements the selection of records from a given set, which is represented a list of entities
-- supposing that the list only contains every entity once. The order of an element is a place
-- taken in the list.
type EntitySet record = [record]


-- Transform a selected set into a list of selected elements separated by the criteria,
-- with order preserving mode.
type Selector record = EntitySet record -> [EntitySet record]


selects :: (PersistEntity record, PersistEntityBackend record ~ FileBackend, RecordContainer cont)
        => [SelectOpt record] -> EntitySet (cont record) -> EntitySet (cont record)
selects []     es = es
selects (s:ss) es = concatMap (selects ss) (selecting s es)


selecting :: (PersistEntity record, PersistEntityBackend record ~ FileBackend, RecordContainer cont)
          => SelectOpt record -> Selector (cont record)
selecting = selectOptAlg
  ascendent
  descendent
  offsetBy
  limitTo

-- The possible orders of the entity elements
data Order = Ascendent | Descendent

-- Algebra on the ordering
orderAlg
  ascendent
  descendent
  o = case o of
    Ascendent  -> ascendent
    Descendent -> descendent

-- Higher level function that parametrizes the selection based on ordering
ordered :: (PersistEntity record, PersistEntityBackend record ~ FileBackend, RecordContainer cont)
        => Order -> EntityField record typ -> Selector (cont record)
ordered ordering = ord . persistFieldDef where
  ord fieldDef es = groupedEntities where
    -- list of value and original entity pair
    xs = zip <$> map (lookup fieldDef . persistEntityValueMap . getRecord) <*> id $ es
    sorted  = sortBy  (on comparator fst) xs
    grouped = groupBy (on (==)       fst) sorted
    groupedEntities = map (map snd) grouped

    comparator = orderAlg compare (flip compare) ordering


-- Orders the given list into an ascendataly ordered list
ascendent :: (PersistEntity record, PersistEntityBackend record ~ FileBackend, RecordContainer cont)
          => EntityField record typ -> Selector (cont record)
ascendent = ordered Ascendent


-- Orders the given list into a descendatally ordered list
descendent :: (PersistEntity record, PersistEntityBackend record ~ FileBackend, RecordContainer cont)
           => EntityField record typ -> Selector (cont record)
descendent = ordered Descendent


-- Drops the first d elements of the list
offsetBy :: Int -> Selector record
offsetBy d = wrap . drop d


-- Takes the first t elements of the list
limitTo :: Int -> Selector record
limitTo t = wrap . take t


-- Takes one element and wraps it into a list
wrap e = [e]


-- Looks up a given field in the Map,
-- If the field is found returns the associated value
-- Otherwise throws an FBE_FieldDoesNotExist exception
lookup k = fromMaybe (throw . FBE_FieldDoesNotExist . Text.pack $ show k) . Map.lookup k
