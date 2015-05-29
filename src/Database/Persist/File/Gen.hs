{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.Persist.File.Gen where

import           Control.Applicative
import           Control.Monad.State.Class
import           Database.Persist.Sql hiding (update, updateField)
import           Control.Monad.Trans.State
import           Data.Text (Text)
import qualified Data.Text as Text

import           Database.Persist.File.Base
import qualified Database.Persist.File.Shape as Shape

import           Test.QuickCheck.Gen
import           Test.QuickCheck.Arbitrary

class GenString s where
  emptyStr    :: Gen s
  oneLetter   :: Gen s
  manyLetters :: Gen s

instance GenString String where
  emptyStr    = return []
  oneLetter   = wrap <$> arbitrary
  manyLetters = listOf1 arbitrary

instance GenString Text where
  emptyStr    = Text.pack <$> emptyStr
  oneLetter   = Text.pack <$> oneLetter
  manyLetters = Text.pack <$> manyLetters

instance GenString HaskellName where
  emptyStr    = HaskellName <$> emptyStr
  oneLetter   = HaskellName <$> oneLetter
  manyLetters = HaskellName <$> manyLetters

instance GenString DBName where
  emptyStr    = DBName <$> emptyStr
  oneLetter   = DBName <$> oneLetter
  manyLetters = DBName <$> manyLetters

instance Arbitrary Text where
  arbitrary = Text.pack <$> arbitrary
  shrink = map Text.pack . shrink . Text.unpack

instance Arbitrary PersistUpdate where
  arbitrary = elements [Assign, Add, Subtract, Multiply, Divide]
  shrink = persistUpdate
             [Add, Subtract, Multiply, Divide]
             [Assign, Subtract, Multiply, Divide]
             [Assign, Add, Multiply, Divide]
             [Assign, Add, Subtract, Divide]
             [Assign, Add, Subtract, Multiply]

instance Arbitrary PersistFilter where
  arbitrary = frequency [
      let xs = [Eq, Ne, Gt, Lt, Ge, Le, In, NotIn]
      in (length xs, elements xs)
    , (1, BackendSpecificFilter . Text.pack <$> arbitrary)
    ]

instance Arbitrary FieldType where
  arbitrary = sized fieldTypeGen

fieldTypeGen :: Int -> Gen FieldType
fieldTypeGen 0 = FTTypeCon <$> arbitrary <*> arbitrary
fieldTypeGen n = oneof
  [ FTApp  <$> fieldTypeGen (div n 2) <*> fieldTypeGen (div n 2)
  , FTList <$> fieldTypeGen (div n 2)
  ]

instance GenString UniqueDef where
  emptyStr  = UniqueDef <$> emptyStr
                        <*> emptyStr
                        <*> listOf (pair <$> emptyStr <*> emptyStr)
                        <*> listOf emptyStr
  oneLetter = UniqueDef <$> oneLetter
                        <*> oneLetter
                        <*> listOf (pair <$> oneLetter <*> oneLetter)
                        <*> listOf oneLetter
  manyLetters = UniqueDef <$> manyLetters
                          <*> manyLetters
                          <*> listOf (pair <$> manyLetters <*> manyLetters)
                          <*> listOf manyLetters

fieldTypeGen2 :: Int -> Gen FieldType
fieldTypeGen2 = Shape.hyloM (return . getType) genType
  where
    genType n
      | n < 2     = Shape.FTTypeCon <$> arbitrary <*> arbitrary
      | otherwise =
          let n2 = div n 2
          in elements [ Shape.FTApp n2 n2, Shape.FTList n2 ]

    getType (Shape.FTTypeCon m n) = FTTypeCon m n
    getType (Shape.FTApp f t)     = FTApp f t
    getType (Shape.FTList t)      = FTList t

fromInt :: Gen a -> Int -> Gen (Shape.List a Int)
fromInt gen n
  | n <= 0    = return Shape.Nil
  | otherwise = Shape.Cons <$> gen <*> (return (n - 1))

fromList :: Shape.List a [a] -> Gen [a]
fromList Shape.Nil         = return []
fromList (Shape.Cons x xs) = return (x:xs)

genFromInt = Shape.hyloM fromList (fromInt ints)
  where
    ints :: Gen Int
    ints = arbitrary

