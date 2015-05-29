{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
module Database.Persist.File.Base (
    module Database.Persist.Class
  , module Database.Persist.Types
  , updateAlg
  , persistUpdate
  , referenceDef
  , filter
  , persistFilter
  , selectOpt
  , attr
  , haskellName
  , dbName
  , uniqueDef
  , persistValueAlg
  , pair
  , wrap
  , getDBName
  ) where

import Prelude hiding (filter, writeFile)

import           Data.Aeson as Aeson
import           Data.Bifunctor
import           Data.Char
import           Data.List (groupBy, intersperse)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as Text
import           Database.Persist.Class
import           Database.Persist.Sql hiding (update, updateField)
import           Database.Persist.Types
import           Database.Persist.TH
import           Web.PathPieces

import           Control.Applicative ((<$>), (<*>))
import           Control.Arrow
import           Control.DeepSeq (deepseq)
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.String
import qualified Data.Text as Text
import           Data.Typeable
import           System.IO hiding (writeFile)

import           Test.QuickCheck

import           Database.Persist.File.Directory


-- * Interfacing with the persitent API

-- type Attr = Text

attr :: (Text -> a) -> Attr -> a
attr f attr = f attr

--newtype HaskellName = HaskellName { unHaskellName :: Text }
--    deriving (Show, Eq, Read, Ord)

haskellName :: (Text -> a) -> HaskellName -> a
haskellName f (HaskellName name) = f name

--newtype DBName = DBName { unDBName :: Text }
--    deriving (Show, Eq, Read, Ord)

dbName :: (Text -> a) -> DBName -> a
dbName f (DBName name) = f name

getDBName = dbName Text.unpack

{-
-- Persistent users use combinators to create these
data Update record = forall typ. PersistField typ => Update
    { updateField :: EntityField record typ
    , updateValue :: typ
    -- FIXME Replace with expr down the road
    , updateUpdate :: PersistUpdate
    }
    | BackendUpdate
          (BackendSpecificUpdate (PersistEntityBackend record) record)
-}

updateAlg :: (forall typ . PersistField typ => EntityField record typ -> typ -> PersistUpdate -> a)
          -> (b -> a) -> (BackendSpecificUpdate (PersistEntityBackend record) record -> b)
          -> Update record
          -> a
updateAlg
  update
  backendUpdate backendSpecificUpdate
  u = case u of
    Update field value update_ -> update field value update_
    BackendUpdate bsu -> backendUpdate (backendSpecificUpdate bsu)

persistUpdate
  assign
  add
  substract
  multiply
  divide
  p = case p of
    Assign   -> assign
    Add      -> add
    Subtract -> substract
    Multiply -> multiply
    Divide   -> divide

referenceDef
  noReference
  foreignRef   haskellName fieldType
  embedRef     embedEntityDef
  compositeRef compositeDef
  r = case r of
    NoReference      -> noReference
    ForeignRef hn ft -> foreignRef (haskellName hn) (fieldType ft)
    EmbedRef e       -> embedRef (embedEntityDef e)
    CompositeRef c   -> compositeRef (compositeDef c)

{-
-- Persistent users use combinators to create these
data Filter record = forall typ. PersistField typ => Filter
    { filterField  :: EntityField record typ
    , filterValue  :: Either typ [typ] -- FIXME
    , filterFilter :: PersistFilter -- FIXME
    }
    | FilterAnd [Filter record] -- ^ convenient for internal use, not needed for the API
    | FilterOr  [Filter record]
    | BackendFilter
          (BackendSpecificFilter (PersistEntityBackend record) record)
-}


filter :: (forall typ . PersistField typ => EntityField record typ -> Either typ [typ] -> PersistFilter -> a)
       -> (b -> a) -> ([a] -> b)
       -> (b -> a) -> ([a] -> b)
       -> (c -> a) -> ((BackendSpecificFilter (PersistEntityBackend record) record) -> c)
       -> Filter record
       -> a
filter
  filter_
  filterAnd filterAndList
  filterOr  filterOrList
  backendFilter backendSpecificFilter
  f = case f of
    Filter filterField filterValue filterFilter -> filter_ filterField filterValue filterFilter
    FilterAnd fs    -> filterAnd . filterAndList $ map filter' fs
    FilterOr  fs    -> filterOr  . filterOrList  $ map filter' fs
    BackendFilter b -> backendFilter (backendSpecificFilter b)
  where
    filter' = filter
      filter_
      filterAnd filterAndList
      filterOr  filterOrList
      backendFilter backendSpecificFilter

{-
data PersistFilter
  = Eq
  | Ne
  | Gt
  | Lt
  | Ge
  | Le
  | In
  | NotIn
  | BackendSpecificFilter Text
-}

persistFilter
  eq
  ne
  gt
  lt
  ge
  le
  in_
  notIn
  backendSpecificFilter
  f = case f of
    Eq -> eq
    Ne -> ne
    Gt -> gt
    Lt -> lt
    Ge -> ge
    Le -> le
    In -> in_
    NotIn -> notIn
    BackendSpecificFilter text -> backendSpecificFilter text

{-
-- Persistent users use these directly
data SelectOpt record = forall typ. Asc  (EntityField record typ)
                      | forall typ. Desc (EntityField record typ)
                      | OffsetBy Int
                      | LimitTo Int
-}

selectOpt
  :: (forall typ . EntityField record typ -> a)
  -> (forall typ . EntityField record typ -> a)
  -> (Int -> a)
  -> (Int -> a)
  -> SelectOpt record
  -> a
selectOpt
  asc
  desc
  offsetBy
  limitTo
  s = case s of
    Asc  field -> asc  field
    Desc field -> desc field
    OffsetBy o -> offsetBy o
    LimitTo  o -> limitTo  o

{-
data FieldType
    = FTTypeCon (Maybe Text) Text -- ^ optional module, name
    | FTApp FieldType FieldType
    | FTList FieldType
  deriving (Show, Eq, Read, Ord)
-}

fieldTypeAlg
  con
  app
  lst
  f = case f of
    FTTypeCon mdl name -> con mdl name
    FTApp f1 f2 -> app (fieldTypeAlg' f1) (fieldTypeAlg' f2)
    FTList f -> lst (fieldTypeAlg' f)
  where
    fieldTypeAlg' = fieldTypeAlg con app lst

{-
data UniqueDef = UniqueDef
    { uniqueHaskell :: !HaskellName
    , uniqueDBName  :: !DBName
    , uniqueFields  :: ![(HaskellName, DBName)]
    , uniqueAttrs   :: ![Attr]
    }
    deriving (Show, Eq, Read, Ord)
-}

uniqueDef
  :: (HaskellName -> DBName -> [(HaskellName, DBName)] -> [Attr] -> a)
  -> UniqueDef
  -> a
uniqueDef f (UniqueDef name dbname fields attrs) = f name dbname fields attrs


{-
data EntityDef = EntityDef
    { entityHaskell :: !HaskellName
    , entityDB      :: !DBName
    , entityId      :: !FieldDef
    , entityAttrs   :: ![Attr]
    , entityFields  :: ![FieldDef]
    , entityUniques :: ![UniqueDef]
    , entityForeigns:: ![ForeignDef]
    , entityDerives :: ![Text]
    , entityExtra   :: !(Map Text [ExtraLine])
    , entitySum     :: !Bool
    }
    deriving (Show, Eq, Read, Ord)
-}

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

persistValueAlg
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
  p = case p of
    PersistText text -> persistText text
    PersistByteString byteString -> persistByteString byteString
    PersistInt64 int64 -> persistInt64 int64
    PersistDouble double -> persistDouble double
    PersistRational rational -> persistRational rational
    PersistBool bool -> persistBool bool
    PersistDay day -> persistDay day
    PersistTimeOfDay timeOfDay -> persistTimeOfDay timeOfDay
    PersistUTCTime utcime -> persistUTCTime utcime
    PersistNull -> persistNull
    PersistList persistValueList -> persistList $ map persistValueAlg' persistValueList
    PersistMap persistValueMap -> persistMap $ mapOnSnd persistValueAlg' persistValueMap
    PersistObjectId byteString -> persistObjectId byteString
    PersistDbSpecific byteString -> persistDbSpecific byteString
  where
    mapOnSnd f = map (\(x,y) -> (x, f y))
    persistValueAlg' =
      persistValueAlg
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

pair a b = (a, b)

wrap x = [x]
