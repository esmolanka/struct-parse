{-# LANGUAGE OverloadedStrings #-}

import Data.TomlObject
import Control.StructParser

main :: IO ()
main = do
  r <- either (error . show) id <$> readObjectFromFile "example.toml"
  print =<< parseIO (runParsing (context "Inside The Object" >>> key "foo" >>> key "bar" >>> int)) r
