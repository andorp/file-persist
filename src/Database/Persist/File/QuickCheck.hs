{-# LANGUAGE DeriveDataTypeable #-}
--{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Database.Persist.File.QuickCheck where

import           Prelude hiding (length)

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

import           Test.QuickCheck

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

class Length k where
  length :: k -> Int

emptyNonEmptyLabel k = case length k of
  0 -> "Empty"
  1 -> "One"
  _ -> "Non Empty"

-- * Test Cases

-- Checks if the From To JSON is an inverse
fromJSONtoJSON_inverse :: (Eq key, Show key, Arbitrary key, FromJSON key, ToJSON key, Length key) => key -> Property
fromJSONtoJSON_inverse = labelEqClasses emptyNonEmptyLabel $ (\k -> (Aeson.Success k) === (fromJSON $ toJSON k))

-- Checks if the From To PersistValue is an inverse
persistValue_inverse :: (Eq key, Show key, Arbitrary key, PersistField key, Length key) => key -> Property
persistValue_inverse = labelEqClasses emptyNonEmptyLabel $ (\k -> (Right k) === (fromPersistValue $ toPersistValue k))

