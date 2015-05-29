{-# LANGUAGE UndecidableInstances #-} -- For the Show (Fix f) instance
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LambdaCase #-}
module Database.Persist.File.Shape where

import Prelude hiding (mapM)
import Control.Monad hiding (mapM)
import Data.Foldable
import Data.Traversable
import Data.Text

newtype Fix f = Fix { unFix :: f (Fix f) }

-- This requires UndecidableInstances because the context is larger
-- than the head and so GHC can't guarantee that the instance safely
-- terminates. It is in fact safe, however.
instance (Show (f (Fix f))) => Show (Fix f) where
    show (Fix f) = show f

instance (Eq (f (Fix f))) => Eq (Fix f) where
    Fix x == Fix y  =  x == y
    Fix x /= Fix y  =  x /= y

instance (Ord (f (Fix f))) => Ord (Fix f) where
    Fix x `compare` Fix y  =  x `compare` y
    Fix x >  Fix y         =  x >  y
    Fix x >= Fix y         =  x >= y
    Fix x <= Fix y         =  x <= y
    Fix x <  Fix y         =  x <  y
    Fix x `max` Fix y      =  Fix (max x y)
    Fix x `min` Fix y      =  Fix (min x y)

----------------------------------------------------------------
-- | A pure catamorphism over the least fixed point of a functor.
-- This function applies the @f@-algebra from the bottom up over
-- @Fix f@ to create some residual value.
cata :: (Functor f) => (f a -> a) -> (Fix f -> a)
cata phi = self where
  self = phi . fmap self . unFix
{-# INLINE [0] cata #-}

-- | A catamorphism for monadic @f@-algebras. Alas, this isn't wholly
-- generic to @Functor@ since it requires distribution of @f@ over
-- @m@ (provided by 'sequence' or 'mapM' in 'Traversable').
--
-- N.B., this orders the side effects from the bottom up.
cataM :: (Traversable f, Monad m) => (f a -> m a) -> (Fix f -> m a)
cataM phiM = self
    where
    self = phiM <=< (mapM self . unFix)
{-# INLINE cataM #-}

----------------------------------------------------------------
-- | A pure anamorphism generating the greatest fixed point of a
-- functor. This function applies an @f@-coalgebra from the top
-- down to expand a seed into a @Fix f@.
ana :: (Functor f) => (a -> f a) -> (a -> Fix f)
ana psi = self where
  self = Fix . fmap self . psi
{-# INLINE [0] ana #-}

-- | An anamorphism for monadic @f@-coalgebras. Alas, this isn't
-- wholly generic to @Functor@ since it requires distribution of
-- @f@ over @m@ (provided by 'sequence' or 'mapM' in 'Traversable').
--
-- N.B., this orders the side effects from the top down.
anaM :: (Traversable f, Monad m) => (a -> m (f a)) -> (a -> m (Fix f))
anaM psiM = self
    where
    self = (liftM Fix . mapM self) <=< psiM
{-# INLINE anaM #-}

----------------------------------------------------------------
-- Is this even worth mentioning? We can amortize the construction
-- of @Fix f@ (which we'd do anyways because of laziness), but we
-- can't fuse the @f@ away unless we inline all of @psi@, @fmap@,
-- and @phi@ at the use sites. Will inlining this definition be
-- sufficient to do that?

-- | @hylo phi psi == cata phi . ana psi@
hylo :: (Functor f) => (f b -> b) -> (a -> f a) -> (a -> b)
hylo phi psi = self
    where
    self = phi . fmap self . psi
{-# INLINE hylo #-}

-- | @hyloM phiM psiM == cataM phiM <=< anaM psiM@
hyloM :: (Traversable f, Monad m)
      => (f b -> m b) -> (a -> m (f a)) -> (a -> m b)
hyloM phiM psiM = self
    where
    self = phiM <=< mapM self <=< psiM
{-# INLINE hyloM #-}
