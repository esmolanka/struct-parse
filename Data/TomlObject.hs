{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.TomlObject where

import Prelude hiding (id, (.))
import Control.Arrow
import Control.Category
import Control.Applicative
import Control.StructParser
import Control.StructParser.Types

import Data.Functor.Foldable (Fix (..))
import Data.Int
import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Traversable as Tr

import Text.Parsec.Error (ParseError)
import Text.Toml
import Text.Toml.Types

import Data.Object.Types
import Data.Object.Parse

data Scalar
  = SString Text
  | SInteger Int64
  | SFloat Double
  | SBoolean Bool
  | SDatetime UTCTime
    deriving (Show, Eq, Ord)

instance GetId Scalar where
  getId (SString _) = Id "String"
  getId (SInteger _) = Id "Integer"
  getId (SFloat _) = Id "Float"
  getId (SBoolean _) = Id "Boolean"
  getId (SDatetime _) = Id "Datetime"

type TomlObject = Object Text Scalar
type AnnotatedTomlObject = AnnotatedObject Text Scalar

instance FieldKey Text where
  fieldQualifier s = InField (T.unpack s)
  fieldIdentifier s = Id $ "." ++ (T.unpack s)

withInteger :: (Int64 -> Parser a) -> AnnotatedTomlObject -> Parser a
withInteger p = withScalar (Id "Integer") go
  where
    go (SInteger n) = p n
    go s = expectationError (Id "Integer") (getId $ Scalar s)

withFloat :: (Double -> Parser a) -> AnnotatedTomlObject -> Parser a
withFloat p = withScalar (Id "Float") go
  where
    go (SFloat n) = p n
    go s = expectationError (Id "Float") (getId $ Scalar s)

withString :: (Text -> Parser a) -> AnnotatedTomlObject -> Parser a
withString p = withScalar (Id "String") go
  where
    go (SString s) = p s
    go s = expectationError (Id "Bool") (getId $ Scalar s)

withBool :: (Bool -> Parser a) -> AnnotatedTomlObject -> Parser a
withBool p = withScalar (Id "Bool") go
  where
    go (SBoolean s) = p s
    go s = expectationError (Id "Bool") (getId $ Scalar s)

class FromTOML a where
  parseTOML :: AnnotatedTomlObject -> Parser a

instance FromTOML AnnotatedTomlObject where
  parseTOML = pure

instance FromTOML TomlObject where
  parseTOML = pure . unannotate

instance FromTOML Int where
  parseTOML v = fromIntegral <$> withInteger pure v

instance FromTOML Text where
  parseTOML = withString pure

instance FromTOML String where
  parseTOML = withString (pure . T.unpack)

instance FromTOML Bool where
  parseTOML = withBool pure

instance FromTOML a => FromTOML [a] where
  parseTOML = withArray (withElems parseTOML)

instance FromTOML a => FromTOML (HM.HashMap Text a) where
  parseTOML = withObject (Tr.traverse parseTOML)

instance FromTOML a => FromTOML (M.Map Text a) where
  parseTOML = withObject (fmap (M.fromList . HM.toList) . Tr.traverse parseTOML)

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

parseObject :: FilePath -> Text -> Either ParseError (Object Text Scalar)
parseObject identifier body =
  transformTable <$> parseTomlDoc identifier body

readObjectFromFile :: FilePath -> IO (Either ParseError (Object Text Scalar))
readObjectFromFile filepath =
  parseObject filepath <$> T.readFile filepath

----------------------------------------------------------------------
-- Parser composer combinators

bool :: Parsing Parser AnnotatedTomlObject Bool
bool = mkParsing withBool

int :: Parsing Parser AnnotatedTomlObject Int64
int = mkParsing withInteger

double :: Parsing Parser AnnotatedTomlObject Double
double = mkParsing withFloat

string :: Parsing Parser AnnotatedTomlObject Text
string = mkParsing withString

index :: Int -> Parsing Parser AnnotatedTomlObject AnnotatedTomlObject
index n = mkParsing (\f -> withArray (withElem n f))

elems :: Parsing Parser AnnotatedTomlObject [AnnotatedTomlObject]
elems = mkParsing withArray

key :: Text -> Parsing Parser AnnotatedTomlObject AnnotatedTomlObject
key k = mkParsing (\f -> withObject (withField k f))

fields :: Parsing Parser AnnotatedTomlObject (HM.HashMap Text AnnotatedTomlObject)
fields = mkParsing withObject

instance Hole Parser AnnotatedTomlObject where
  holes = elems <|> (fields >>> arr HM.elems) <|> pure []
