{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Data.JSONPath.Parser (jsonPathElement, jsonPath) where

import qualified Data.Char as Char
import Data.Functor
import Data.Functor.Identity
import Data.JSONPath.Types
import Data.Scientific (toRealFloat)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Text.Megaparsec as A
import Text.Megaparsec.Char (char, space, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = A.ParsecT Void Text Identity

jsonPath :: Parser a -> Parser [JSONPathElement]
jsonPath endParser = do
  _ <- optional $ char '$'
  manyTill jsonPathElement (hidden $ lookAhead endParser)

jsonPathElement :: Parser JSONPathElement
jsonPathElement =
  do
    try anyChild
    <|> try keyChildDot
    <|> try keyChildBracket
    <|> try slice
    <|> try sliceUnion
    <|> try filterParser
    <|> try search
    <|> searchBeginningWithSlice

slice :: Parser JSONPathElement
slice = Slice <$> ignoreSurroundingSqBr sliceWithoutBrackets

sliceWithoutBrackets :: Parser SliceElement
sliceWithoutBrackets =
  try multipleIndices
    <|> singleIndex

singleIndex :: Parser SliceElement
singleIndex = ignoreSurroundingSpace $ SingleIndex <$> L.signed space L.decimal

multipleIndices :: Parser SliceElement
multipleIndices = do
  MultipleIndices
    <$> parseStart
    <*> parseEnd
    <*> parseStep
  where
    parseStart :: Parser (Maybe Int)
    parseStart =
      ignoreSurroundingSpace (optional (L.signed space L.decimal))
        <* char ':'

    parseEnd =
      ignoreSurroundingSpace $ optional $ L.signed space L.decimal

    parseStep =
      optional (char ':')
        *> ignoreSurroundingSpace (optional (L.signed space L.decimal))

keyChildBracket :: Parser JSONPathElement
keyChildBracket =
  ignoreSurroundingSqBr $
    ignoreSurroundingSpace $
      KeyChild <$> quotedString

keyChildDot :: Parser JSONPathElement
keyChildDot =
  KeyChild
    <$ char '.'
    <*> takeWhile1P Nothing (\c -> Char.isAlphaNum c || c == '-' || c == '_')

anyChild :: Parser JSONPathElement
anyChild = AnyChild <$ (string ".*" <|> string "[*]")

sliceUnion :: Parser JSONPathElement
sliceUnion =
  ignoreSurroundingSqBr $
    SliceUnion
      <$> sliceWithoutBrackets
      <* char ','
      <*> sliceWithoutBrackets

filterParser :: Parser JSONPathElement
filterParser = do
  _ <- string "[?("
  b <- beginningPoint
  js <- jsonPath condition
  c <- condition
  l <- literal
  _ <- string ")]"
  return $ Filter b js c l

search :: Parser JSONPathElement
search = do
  _ <- char '.'
  _ <- lookAhead (char '.')
  Search <$> some jsonPathElement

searchBeginningWithSlice :: Parser JSONPathElement
searchBeginningWithSlice = do
  _ <- string ".."
  _ <- lookAhead (char '[')
  Search <$> some jsonPathElement

beginningPoint :: Parser BeginningPoint
beginningPoint = do
  (char '$' $> Root) <|> (char '@' $> CurrentObject)

condition :: Parser Condition
condition =
  ignoreSurroundingSpace $
    string "==" $> Equal
      <|> string "!=" $> NotEqual
      <|> string "<=" $> SmallerThanOrEqual
      <|> string ">=" $> GreaterThanOrEqual
      <|> string ">" $> GreaterThan
      <|> string "<" $> SmallerThan

literal :: Parser Literal
literal = do
  LitNumber <$> double <|> LitString <$> quotedString

double :: Parser Double
double = toRealFloat <$> L.scientific

ignoreSurroundingSpace :: Parser a -> Parser a
ignoreSurroundingSpace p = space *> p <* space

ignoreSurroundingSqBr :: Parser a -> Parser a
ignoreSurroundingSqBr p = char '[' *> p <* char ']'

quotedString :: Parser Text
quotedString = Text.pack <$> (inQuotes '"' <|> inQuotes '\'')
  where
    inQuotes quoteChar =
      char quoteChar *> manyTill L.charLiteral (char quoteChar)
