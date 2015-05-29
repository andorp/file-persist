{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.Persist.File.BlackBox where

import Control.Applicative
import Control.Monad.State.Class
import Control.Monad.Trans.State

import Database.Persist.File.Base

import Test.QuickCheck.Gen

newtype Partition a = Partition { unPart :: Gen a }
  deriving (Functor, Applicative, Monad)

partFromList :: [a] -> Partition a
partFromList = Partition . elements

newtype Boundary a = Boundary { unBoundary :: Gen a }
  deriving (Functor, Applicative, Monad)

boundaryFromList :: [a] -> Boundary a
boundaryFromList = Boundary . elements

newtype StateTransition s a = StateTransition { unStTrans :: StateT s Gen a }
  deriving (Functor, Applicative, Monad, MonadState s)

fromStateTrans :: s -> StateTransition s a -> Gen (a, s)
fromStateTrans s = flip runStateT s . unStTrans 

-- Converts the paramteric data type to generator
class ToGen g where
  toGen :: g a -> Gen a

instance ToGen [] where
  toGen = elements 

instance ToGen Partition where
  toGen = unPart

instance ToGen Boundary where
  toGen = unBoundary

-- Converts a generator to a parametric data type
class FromGen g where
  fromGen :: Gen a -> g a

instance FromGen Partition where
  fromGen = Partition

instance FromGen Boundary where
  fromGen = Boundary

