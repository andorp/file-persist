{-# LANGUAGE TypeFamilies #-}
module Database.Persist.File.Unique where

import Control.Arrow ((&&&))
import Control.Exception
import Control.Monad (when)

import Data.Hashable
import Data.Fixed
import Data.Maybe
import Data.Ratio
import Data.String (fromString)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import qualified Data.Map as Map
import System.FilePath.Posix ((</>))

import Database.Persist.Class
import Database.Persist.File.Base
import Database.Persist.File.FileBackend

{-
For every table which has some unique keysets, there is a directory
in the metadata directory, we hash the unique values and we create a link
for the actual record, but we need to insert unique values at the insertion
of normal values.
-}

