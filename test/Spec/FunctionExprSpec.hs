{-# LANGUAGE OverloadedStrings #-}

module Spec.FunctionExprSpec (spec) where

import Data.JSONPath.Parser(functionExpr)
import Data.JSONPath.Types
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
  describe "function expression" $ do
    it "parses literal argument" $
      parse (functionExpr eof) "" "foo(true)"
        `shouldParse` FunctionExpr "foo" [ArgLiteral $ CmpBool True]
    it "recovers from match of literal argument" $
      parse (functionExpr eof) "" "foo(true == true)"
        `shouldParse` FunctionExpr "foo" [ArgLogicalExpr $ ComparisonExpr (CmpBool True) Equal (CmpBool True)]
    it "ummatched parenthesis parse error" $
      parse (functionExpr eof) "" "foo(@"
        `shouldFailWith` err 5 (ueof <> functionCallMissingClosingParenthesisEtoks <> elabel "white space")
