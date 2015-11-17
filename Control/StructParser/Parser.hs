{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Control.StructParser.Parser
  ( Parser
  , runParser
  , expectationError
  , missingFieldError
  , parseError
  , withContext
  , dive
  , jump
  , withNode
  , withLeaves
  , withLookup
  , withIndex
  , FailureTree
  , FailureTreeF (..)
  ) where

import Control.Applicative
import Control.Arrow
import Control.Comonad.Cofree
import Control.Monad.Reader

import qualified Data.Set as S

import Control.StructParser.Types

expectationError :: Qualifier -> Qualifier -> Parser a
expectationError expected got =
  mkParseError (Expectation expected (Just got))

missingFieldError :: Qualifier -> Parser a
missingFieldError expected =
  mkParseError (Expectation expected Nothing)

parseError :: String -> Parser a
parseError msg =
  mkParseError (ParseError msg)

-- | Run a parser with a Context annotation
withContext :: String -> (a -> Parser r) -> a -> Parser r
withContext ctx p a =
  localA (second (Context ctx :)) (p a)

type Analyser c a r =
  forall p. (Applicative p, Functor p) => (a -> p r) -> p r -> c -> p r

withNode
  :: (GetId (f (Annotated f))) =>
     Qualifier
  -> Analyser (f (Annotated f)) a r
  -> (a -> Parser r)
  -> Annotated f -> Parser r
withNode expectation analyser cont (pos :< obj) =
  jump pos $ analyser (dive pos . cont) (expectationError expectation identifier) obj
    where
      identifier = getId obj

type IxMapper g k b a = forall a. (k -> b -> a) -> g k b -> g k a

withLeaves
  :: (Traversable (g k), Functor (g k)) =>
     IxMapper g k (Annotated f) a
  -> (k -> Annotated f -> Parser r)
  -> g k (Annotated f)
  -> Parser (g k r)
withLeaves mapper cont = sequenceA . mapper go
  where
    go k (pos :< obj) = jump pos (cont k (pos :< obj))

withLookup
  :: (FieldKey k) =>
     (forall p. (Annotated f -> p r) -> (Qualifier -> p r) -> k -> g k (Annotated f) -> p r)
  -> k
  -> (Annotated f -> Parser r)
  -> g k (Annotated f)
  -> Parser r
withLookup lookup k cont =
  lookup cont missingFieldError k

withIndex
  :: (forall p. (Annotated f -> p r) -> p r -> Int -> g (Annotated f) -> p r)
  -> Int
  -> (Annotated f -> Parser r)
  -> g (Annotated f)
  -> Parser r
withIndex index n cont =
  index cont (missingFieldError $ QIndex n) n

----------------------------------------------------------------------
-- Internal

data Result a
  = Success a
  | Failure FailureTree
    deriving (Functor)

instance Applicative Result where
  pure = Success
  Success f <*> Success a = Success (f a)
  Success _ <*> Failure e = Failure e
  Failure e <*> Success _ = Failure e
  Failure e <*> Failure i = Failure $ mergeFailureTrees Both e i

instance Alternative Result where
  empty = Failure ([] :< ParseError "empty")
  Success a <|> _         = Success a
  Failure _ <|> Success a = Success a
  Failure e <|> Failure i = Failure $ mergeFailureTrees Any e i

instance Monad Result where
  return = Success
  (Success a) >>= fm = fm a
  Failure f >>= _ = Failure f

newtype Parser a = Parser
  { unParser :: ReaderT (Position, [Context]) Result a }
  deriving (Functor, Monad)

instance Applicative Parser where
  pure a = mkParser (\_ -> Success a)
  fa <*> b = mkParser (\env -> runReaderT (unParser fa) env <*> runReaderT (unParser b) env)

mkParseError :: FailureTreeF FailureTree -> Parser a
mkParseError fs =
  mkParser $ \(path, ctx) ->
    Failure . settle (ctx :< fs) . reverse $ path

instance Alternative Parser where
  empty = mkParseError (ParseError "empty")
  a <|> b = mkParser (\env -> runReaderT (unParser a) env <|> runReaderT (unParser b) env)

settle :: FailureTree -> Position -> FailureTree
settle tree = fst . foldr embed (tree, Nothing)
  where
    embed (QObject idn) (tree, _) = (tree, Just (QObject idn))
    embed q (tree, ty) = ([] :< Dive (q, ty)  tree, Nothing)

runParser' :: (Position, [Context]) -> Parser a -> Result a
runParser' p = flip runReaderT p . unParser

runParser :: (b -> Parser a) -> b -> Either FailureTree a
runParser p v = unResult . runParser' ([QObject "@"], []) $ p v
  where unResult (Success a)      = Right a
        unResult (Failure reason) = Left reason

mkParser :: ((Position, [Context]) -> Result a) -> Parser a
mkParser fr = Parser (ReaderT fr)

localA :: ((Position, [Context]) -> (Position, [Context])) -> Parser a -> Parser a
localA f p = mkParser (\env -> runReaderT (unParser p) (f env))

-- | Sets current parsing context to position inside of the parsed object
dive :: (Position, Qualifier) -> Parser a -> Parser a
dive (pos, q) = localA (first (const (q:pos)))

-- | Sets current parsing context to position of object
jump :: (Position, Qualifier) -> Parser a -> Parser a
jump (pos, _) = localA (first (const pos))

----------------------------------------------------------------------
-- Failure tree

data FailureTreeF e
  = And [e]
  | Or  [e]
  | Dive (Qualifier, Maybe Qualifier) e
  | Expectation Qualifier (Maybe Qualifier)
  | ParseError String
    deriving (Functor, Show, Ord, Eq)

type FailureTree = Cofree FailureTreeF [Context]
data MergeOperation = Both | Any deriving (Show, Eq)

-- Paths are stored in reverse order, so reversing of list is required.
commonPrefix :: (Eq a) => [a] -> [a] -> [a]
commonPrefix a b =
  reverse . map snd . takeWhile fst $
    zipWith (\a b -> (a == b, a)) (reverse a) (reverse b)

mergeFailureTrees :: MergeOperation -> FailureTree -> FailureTree -> FailureTree
mergeFailureTrees mergeOp ltree rtree = go mergeOp ltree rtree
  where
    go Any  = goAny
    go Both = goBoth

    goBoth (c :< Dive q l)   (k :< Dive p r) | q == p = commonPrefix c k :< Dive q (goBoth l r)
    goBoth (c :< And lnodes) rtree@(k :< _)     = commonPrefix c k :< (mkNode And $ mergeNodes lnodes (andify rtree))
    goBoth ltree             rtree@(_ :< And{}) = goBoth rtree ltree
    goBoth ltree@(c :< _)    rtree@(k :< _)     = commonPrefix c k :< (mkNode And $ S.fromList [ltree, rtree])

    goAny  (c :< Dive q l)   (k :< Dive p r) | q == p = commonPrefix c k :< Dive q (goAny l r)
    goAny  (c :< Or  lnodes) rtree@(k :< _)     = commonPrefix c k :< (mkNode Or $ mergeNodes lnodes (orify rtree))
    goAny  ltree             rtree@(_ :< Or{})  = goAny rtree ltree
    goAny  ltree@(c :< _)    rtree@(k :< _)     = commonPrefix c k :< (mkNode Or $ S.fromList [ltree, rtree])

    andify (_ :< And nodes) = nodes
    andify other            = [other]
    orify  (_ :< Or nodes)  = nodes
    orify  other            = [other]

    mkNode node alts = case S.toList alts of
      []  -> error "Empty list of node alternatives"
      [_ :< x] -> x
      lst -> node lst

    eqQ (_ :< (Dive q1 _)) (_ :< (Dive q2 _)) = q1 == q2
    eqQ _ _ = False

    mergeNodes [] rs = S.fromList rs
    mergeNodes ls [] = S.fromList ls
    mergeNodes (l:ls) (r:rs)
      | l `eqQ` r = S.insert (go mergeOp l r) (mergeNodes ls rs)
      | l < r     = S.insert l (mergeNodes ls (r:rs))
      | otherwise = S.insert r (mergeNodes (l:ls) rs)
