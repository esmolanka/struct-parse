{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Object.Parse where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Hashable
-- import Control.Comonad.Cofree

import Control.StructParser
import Data.Object.Types
import Data.ConstIndex

----------------------------------------------------------------------
-- Nodes

withObject
  :: (GetId s) =>
     (HashMap k (AnnotatedObject k s) -> Parser a)
  -> AnnotatedObject k s
  -> Parser a
withObject = withNode (QObject "Object") $ \cont err obj ->
  case obj of
    Object pairs -> cont pairs
    _ -> err

withArray
  :: (GetId s) =>
     ([AnnotatedObject k s] -> Parser a)
  -> AnnotatedObject k s
  -> Parser a
withArray = withNode (QObject "Array") $ \cont err obj ->
  case obj of
    Array elems -> cont elems
    _ -> err

withScalar
  :: (GetId s) =>
     Qualifier
  -> (s -> Parser a)
  -> AnnotatedObject k s
  -> Parser a
withScalar expectation = withNode expectation $ \cont err obj ->
  case obj of
    Scalar s -> cont s
    _ -> err

----------------------------------------------------------------------
-- Collections

withFields
  :: (FieldKey k)
  => (k -> AnnotatedObject k s -> Parser a)
  -> HashMap k (AnnotatedObject k s)
  -> Parser (HashMap k a)
withFields = withLeaves HM.mapWithKey

withElems
  :: (AnnotatedObject k s -> Parser a)
  -> [AnnotatedObject k s]
  -> Parser [a]
withElems p vs =
  fmap runConstIndex $
    withLeaves (fmap . ($ ())) (const p) (ConstIndex vs)

----------------------------------------------------------------------
-- Elements

withField
  :: (FieldKey k, Hashable k)
  => k
  -> (AnnotatedObject k s -> Parser a)
  -> HashMap k (AnnotatedObject k s)
  -> Parser a
withField = withLookup $ \cont err k hm ->
  maybe (err (fieldQualifier k)) cont (HM.lookup k hm)

withElem
  :: Int
  -> (AnnotatedObject k s -> Parser a)
  -> [AnnotatedObject k s]
  -> Parser a
withElem = withIndex $ \cont err n ls ->
  if n < length ls then cont (ls !! n)
                   else err
