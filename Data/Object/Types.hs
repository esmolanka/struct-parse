{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TupleSections     #-}

module Data.Object.Types where

import Control.Monad.Reader
import Control.Comonad.Cofree

import Data.Functor.Foldable (Fix (..), cata)
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)

import qualified Data.Foldable as Fo
import qualified Data.Traversable as Tr

import Control.StructParser.Types

data ObjectF k s e
  = Object !(HashMap k e)
  | Array ![e]
  | Scalar !s
    deriving (Show, Eq, Functor, Tr.Traversable, Fo.Foldable)

data PairF k e = k :*: e
  deriving (Eq, Show, Functor, Tr.Traversable, Fo.Foldable)

mkObject :: (FieldKey k) => HashMap k (Object k s) -> Object k s
mkObject = Fix . Object

mkArray :: [Object k s] -> Object k s
mkArray = Fix . Array

mkScalar :: s -> Fix (ObjectF k s)
mkScalar = Fix . Scalar

type Object k s = Raw (ObjectF k s)
type AnnotatedObject k s = Annotated (ObjectF k s)

type Pair k s = PairF k (Object k s)
type AnnotatedPair k s = PairF k (AnnotatedObject k s)

annotateWithIndices
  :: [Reader Position (AnnotatedObject k s)]
  -> Reader Position [AnnotatedObject k s]
annotateWithIndices = mapM (\(i, r) -> local (AtIndex i:) r) . zip [0..]

annotateField
  :: (FieldKey k)
  => HashMap k (Reader Position (AnnotatedObject k s))
  -> Reader Position (HashMap k (AnnotatedObject k s))
annotateField = Tr.sequence . HM.mapWithKey (\k -> local (fieldQualifier k:))

instance (GetId s) => GetId (ObjectF k s e)  where
  getId (Object _) = Id "Object"
  getId (Array _)  = Id "Array"
  getId (Scalar s) = getId s

instance (FieldKey k, GetId s) => WithAnnotation (ObjectF k s) where
  annotate root = runReader (cata alg root) [InObj (Id "@")]
    where
      alg :: (FieldKey k, GetId s) =>
             ObjectF k s (Reader Position (Cofree (ObjectF k s) (Position, Qualifier)))
          -> Reader Position (Cofree (ObjectF k s) (Position, Qualifier))
      alg obj =
        case obj of
          Object pairs -> (:<) <$> ( (,objQ) <$> ask) <*> local (objQ:) (Object <$> annotateField pairs)
          Array vals   -> (:<) <$> ( (,objQ) <$> ask) <*> local (objQ:) (Array <$> annotateWithIndices vals)
          Scalar s     -> (:<) <$> ( (,objQ) <$> ask) <*> local (objQ:) (Scalar <$> pure s)
        where
          objQ = getIn obj

  unannotate = cataAnn (const Fix)
