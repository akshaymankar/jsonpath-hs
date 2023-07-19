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

functionCallMissingArgumentEtoks :: ET Text
functionCallMissingArgumentEtoks =
  etoks "false"
    <> etoks "null"
    <> etoks "true"
    <> etoks "!"
    <> etoks "\""
    <> etoks "$"
    <> etoks "'"
    <> etoks "("
    <> etoks "@"

spec :: Spec
spec = do
  describe "function expression" $ do
    it "parses literal argument" $
      parse (functionExpr eof) "" "foo(true)"
        `shouldParse` FunctionExpr "foo" [ArgLiteral $ LitBool True]
    it "parses more than one argument" $
      parse (functionExpr eof) "" "foo( true, $.foo )"
        `shouldParse` FunctionExpr "foo" [ArgLiteral $ LitBool True, ArgFilterQuery $ FilterQuery Root [KeyChild "foo"]]
    it "parses comparison argument" $
      parse (functionExpr eof) "" "foo(true == true)"
        `shouldParse` FunctionExpr "foo" [ArgLogicalExpr $ ComparisonExpr (CmpLiteral $ LitBool True) Equal (CmpLiteral $ LitBool True)]
    it "parses function expression argument" $
      parse (functionExpr eof) "" "foo(bar(@), baz(true))"
        `shouldParse` FunctionExpr "foo" [ArgFunctionExpr $ FunctionExpr "bar" [ArgFilterQuery $ FilterQuery CurrentObject []],
                                          ArgFunctionExpr $ FunctionExpr "baz" [ArgLiteral $ LitBool True]]
    it "ummatched parenthesis parse error" $
      parse (functionExpr eof) "" "foo(@"
        `shouldFailWith` err 5 (ueof <> functionCallMissingClosingParenthesisEtoks <> elabel "white space")
    it "missing argument parse error" $
      parse (functionExpr eof) "" "foo(, true)"
        `shouldFailWith` err 4 (utoks ", tru" <> functionCallMissingArgumentEtoks <> elabel "digit" <> elabel "lowercase character" <> elabel "white space")
