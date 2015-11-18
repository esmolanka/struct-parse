{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.TomlObject
  ( TomlObject
  , AnnotatedTomlObject
  , StructParser
  , parseToml
  , readFromFile
  , parseFromFile
  , int
  , double
  , bool
  , string
  , fields
  , elems
  , key
  , index
  , context
  , this
  , FromToml (..)
  -- * Re-exporting for convenience
  , module Control.Parsing
  , arr
  , Alternative (..)
  ) where

import Prelude hiding (id, (.))
import Control.Arrow
import Control.Category
import Control.Applicative
import Control.StructParser
import Control.Parsing

import Data.Functor.Foldable (Fix (..))
import Data.Int
import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Text.Toml
import Text.Toml.Types

import Data.Object.Types
import Data.Object.Parse

----------------------------------------------------------------------
-- Main interface

parseToml :: StructParser AnnotatedTomlObject a -> TomlObject -> Either String a
parseToml p = parsePretty (runParsing p)

readFromFile :: FilePath -> IO (Either String TomlObject)
readFromFile filepath =
  left show . parseObject filepath <$> T.readFile filepath
    where
      parseObject identifier body =
        transformTable <$> parseTomlDoc identifier body

parseFromFile :: FilePath -> StructParser AnnotatedTomlObject a -> IO (Either String a)
parseFromFile filepath parser =
  (>>= parseToml parser) <$> readFromFile filepath

----------------------------------------------------------------------
-- Data definitions

data Scalar
  = SString Text
  | SInteger Int64
  | SFloat Double
  | SBoolean Bool
  | SDatetime UTCTime
    deriving (Show, Eq, Ord)

instance GetId Scalar where
  getId (SString _) = QObject "String"
  getId (SInteger _) = QObject "Integer"
  getId (SFloat _) = QObject "Float"
  getId (SBoolean _) = QObject "Boolean"
  getId (SDatetime _) = QObject "Datetime"

type TomlObject = Object Text Scalar
type AnnotatedTomlObject = AnnotatedObject Text Scalar

withInteger :: (Int64 -> Parser a) -> AnnotatedTomlObject -> Parser a
withInteger p = withScalar (QObject "Integer") go
  where
    go (SInteger n) = p n
    go s = expectationError (QObject "Integer") (getId $ Scalar s)

withFloat :: (Double -> Parser a) -> AnnotatedTomlObject -> Parser a
withFloat p = withScalar (QObject "Float") go
  where
    go (SFloat n) = p n
    go s = expectationError (QObject "Float") (getId $ Scalar s)

withString :: (Text -> Parser a) -> AnnotatedTomlObject -> Parser a
withString p = withScalar (QObject "String") go
  where
    go (SString s) = p s
    go s = expectationError (QObject "String") (getId $ Scalar s)

withBool :: (Bool -> Parser a) -> AnnotatedTomlObject -> Parser a
withBool p = withScalar (QObject "Bool") go
  where
    go (SBoolean s) = p s
    go s = expectationError (QObject "Bool") (getId $ Scalar s)

----------------------------------------------------------------------
-- Parsing from string

transformTable :: Table -> Object Text Scalar
transformTable = Fix . Object . HM.map transformNode

transformNode :: Node -> Object Text Scalar
transformNode node =
  case node of
    NTValue scalar -> transformScalar scalar
    NTable table   -> transformTable table
    NTArray tables -> Fix $ Array $ map transformTable tables

transformScalar :: TValue -> Object Text Scalar
transformScalar tval = Fix $
  case tval of
    VString str   -> Scalar $ SString str
    VInteger int  -> Scalar $ SInteger int
    VFloat dbl    -> Scalar $ SFloat dbl
    VBoolean bool -> Scalar $ SBoolean bool
    VDatetime ts  -> Scalar $ SDatetime ts
    VArray values -> Array  $ map transformScalar values

----------------------------------------------------------------------
-- Parser composer combinators


bool :: StructParser AnnotatedTomlObject Bool
bool = mkParsing withBool

int :: StructParser AnnotatedTomlObject Int64
int = mkParsing withInteger

double :: StructParser AnnotatedTomlObject Double
double = mkParsing withFloat

string :: StructParser AnnotatedTomlObject Text
string = mkParsing withString

index :: Int -> StructParser AnnotatedTomlObject AnnotatedTomlObject
index n = mkParsing (\f -> withArray (withElem n f))

elems :: StructParser AnnotatedTomlObject [AnnotatedTomlObject]
elems = mkParsing withArray

key :: String -> StructParser AnnotatedTomlObject AnnotatedTomlObject
key k = mkParsing (\f -> withObject (withField (T.pack k) f))

fields :: StructParser AnnotatedTomlObject (HM.HashMap Text AnnotatedTomlObject)
fields = mkParsing withObject

context :: String -> StructParser a a
context ctx = mkParsing (withContext ctx)

this :: StructParser a a
this = id

class FromToml a where
  fromToml :: StructParser AnnotatedTomlObject a

instance FromToml String where
  fromToml = string >>> arr T.unpack

instance FromToml T.Text where
  fromToml = string

instance FromToml Int where
  fromToml = int >>> arr fromIntegral

instance FromToml Integer where
  fromToml = int >>> arr fromIntegral

instance FromToml Double where
  fromToml = double

instance FromToml Bool where
  fromToml = bool

instance (FromToml a) => FromToml (M.Map String a) where
  fromToml =
    fields
    >>> traversing fromToml
    >>> arr (M.fromList . map (first T.unpack) . HM.toList)

instance (FromToml a) => FromToml [a] where
  fromToml =
    elems
    >>> traversing fromToml

instance Hole Parser AnnotatedTomlObject where
  holes = elems <|> (fields >>> arr HM.elems) <|> pure []
