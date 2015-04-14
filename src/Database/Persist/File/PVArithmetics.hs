module Database.Persist.File.PVArithmetics where

import           Prelude as P
import           Database.Persist.Sql (PersistValue(..))

{-
data PersistValue = PersistText Text
                  | PersistByteString ByteString
                  | PersistInt64 Int64
                  | PersistDouble Double
                  | PersistRational Rational
                  | PersistBool Bool
                  | PersistDay Day
                  | PersistTimeOfDay TimeOfDay
                  | PersistUTCTime UTCTime
                  | PersistNull
                  | PersistList [PersistValue]
                  | PersistMap [(Text, PersistValue)]
                  | PersistObjectId ByteString -- ^ Intended especially for MongoDB backend
                  | PersistDbSpecific ByteString -- ^ Using 'PersistDbSpecific' allows you to use types specific to a particular backend
-}

add (PersistInt64 x)  (PersistInt64 y)  = PersistInt64 (x + y)
add (PersistDouble x) (PersistDouble y) = PersistDouble (x + y)
add x y = error $ concat ["ERROR: (", show x, ") + (", show y, ")"]

sub (PersistInt64 x)  (PersistInt64 y)  = PersistInt64 (x - y)
sub (PersistDouble x) (PersistDouble y) = PersistDouble (x - y)
sub x y = error $ concat ["ERROR: (", show x, ") - (", show y, ")"]

mul (PersistInt64 x)  (PersistInt64 y)  = PersistInt64 (x * y)
mul (PersistDouble x) (PersistDouble y) = PersistDouble (x * y)
mul x y = error $ concat ["ERROR: (", show x, ") * (", show y, ")"]

div (PersistInt64 x)  (PersistInt64 y)  = PersistInt64 (P.div x y)
div (PersistDouble x) (PersistDouble y) = PersistDouble (x / y)
div x y = error $ concat ["ERROR: (", show x, ") / (", show y, ")"]

