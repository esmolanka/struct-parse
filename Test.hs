{-# LANGUAGE OverloadedStrings #-}

import Data.TomlObject
import Control.StructParser

test :: Show a => Parsing Parser AnnotatedTomlObject a -> IO ()
test p = do
  r <- either (error . show) id <$> readFromFile "example.toml"
  maybe (return ()) print =<< parseIO (runParsing p) r

main :: IO ()
main = do
  test $ key "database" >>> key "ports" >>> elems
