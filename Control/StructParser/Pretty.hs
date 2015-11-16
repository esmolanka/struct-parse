{-# LANGUAGE OverloadedStrings #-}

module Control.StructParser.Pretty
  ( parsePretty
  , parseIO
  ) where

import System.IO
import Control.Monad.Reader
import Control.Arrow (left)
import Text.PrettyPrint.ANSI.Leijen as PP

import Control.StructParser.Types
import Control.StructParser.Parser

ppInContext :: [Context] -> Doc -> Reader [Context] Doc
ppInContext [] doc = return doc
ppInContext ctxs@(Context s : _) doc = do
  upperContext <- ask
  let context = green . brackets . hcat . punctuate "/" $ map (\(Context s) -> text s) ctxs
      doc' = vcat [context, doc]
  case upperContext of
    [] -> return doc'
    (Context z : _)
      | s == z    -> return doc
      | otherwise -> return doc'

ppBlock :: [Doc] -> Doc
ppBlock [] = empty
ppBlock (a:as) = braces $ (vcat $ (empty <+> align a) : map ((comma <+>) . align) as) <> linebreak

ppFailureTree :: FailureTree -> Doc
ppFailureTree tree = "Failure:" <> linebreak <> failure <> linebreak
  where
    failure = runReader (cataAnn prettyAlg tree) []
    prettyAlg :: [Context] -> FailureTreeF (Reader [Context] Doc) -> Reader [Context] Doc
    prettyAlg c (Dive (q, mty) mdoc) = do
      doc <- local (const c) mdoc
      let tydoc = maybe empty (\idn -> parens (pretty idn)) mty
      ppInContext c (yellow (pretty q <> tydoc <> colon) <+> doc)
    prettyAlg c (Expectation exp Nothing) =
      ppInContext c ("required" <+> pretty exp)
    prettyAlg c (Expectation exp (Just got)) =
      ppInContext c ("expected" <+> pretty exp <> comma <+> "got" <+> pretty got)
    prettyAlg c (ParseError msg) =
      ppInContext c ("error:" <+> text msg)
    prettyAlg c (And mdocs) = do
      docs <- mapM (local (const c)) mdocs
      ppInContext c ("all of" PP.<$> align (ppBlock docs))
    prettyAlg c (Or mdocs) = do
      docs <- mapM (local (const c)) mdocs
      ppInContext c ("any of" PP.<$> align (ppBlock docs))

parsePretty :: (WithAnnotation f) => (Annotated f -> Parser a) -> Raw f -> Either String a
parsePretty p a = left (flip displayS "" . renderPretty 0.3 120 . ppFailureTree) (runParser p (annotate a))

parseIO :: (WithAnnotation f, Show a) => (Annotated f -> Parser a) -> Raw f -> IO (Maybe a)
parseIO p a =
  case runParser p (annotate a) of
    Left reason -> do
      displayIO stderr . renderPretty 0.3 120 $ ppFailureTree reason
      return Nothing
    Right a ->
      return $ Just a

