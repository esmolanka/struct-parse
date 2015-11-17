{-# LANGUAGE OverloadedStrings #-}

import Data.TomlObject
import Control.StructParser

test :: Show a => Parsing Parser AnnotatedTomlObject a -> IO ()
test p = do
  r <- either (error . show) id <$> readObjectFromFile "example.toml"
  print =<< parseIO (runParsing p) r

main :: IO ()
main = do
  test $ context "Inside The Object" >>> key "foo" >>> key "bar" >>> int
