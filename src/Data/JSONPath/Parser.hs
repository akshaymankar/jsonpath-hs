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
    <|> try indexChild
    <|> try union
    <|> try filterParser
    <|> try search
    <|> searchBeginningWithSlice

indexChild :: Parser JSONPathElement
indexChild = IndexChild <$> ignoreSurroundingSqBr indexChildWithoutBrackets

indexChildWithoutBrackets :: Parser Int
indexChildWithoutBrackets = ignoreSurroundingSpace $ L.signed space L.decimal

slice :: Parser JSONPathElement
slice =
  uncurry3 Slice
    <$> ignoreSurroundingSqBr sliceWithoutBrackets

sliceWithoutBrackets :: Parser (Maybe Int, Maybe Int, Maybe Int)
sliceWithoutBrackets = do
  (,,)
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

union :: Parser JSONPathElement
union =
  ignoreSurroundingSqBr $
    Union <$> do
      firstElement <- unionElement
      restElements <- some (char ',' *> unionElement)
      pure (firstElement : restElements)

unionElement :: Parser UnionElement
unionElement =
  try (uncurry3 UESlice <$> sliceWithoutBrackets)
    <|> try (UEIndexChild <$> indexChildWithoutBrackets)
    <|> UEKeyChild <$> ignoreSurroundingSpace quotedString

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

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c
