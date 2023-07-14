{-# LANGUAGE OverloadedStrings #-}

module Spec.CountFunctionSpec (spec) where

import Data.JSONPath.Parser(jsonPath)
import Data.Text
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

functionCallMissingClosingParenthesisEtoks :: ET Text
functionCallMissingClosingParenthesisEtoks =
  etoks ".*"
    <> etoks ".."
    <> etoks ")"
    <> etoks ","
    <> etoks "."
    <> etoks "["

spec :: Spec
spec = do
  describe "count function extension" $ do
    it "ummatched parenthesis parse error" $
      parse (jsonPath eof) "" "$[?count(@"
        `shouldFailWith` err 10 (ueof <> functionCallMissingClosingParenthesisEtoks <> elabel "white space")
