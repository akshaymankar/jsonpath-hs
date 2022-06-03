{-# LANGUAGE OverloadedStrings #-}

module Data.JSONPath.Parser (jsonPathElement, jsonPath) where

import Control.Applicative (optional, (<|>))
import Data.Attoparsec.Text as A
import Data.Functor
import Data.JSONPath.Types
import Data.Text (Text)

jsonPath :: Parser [JSONPathElement]
jsonPath = do
  _ <- skip (== '$') <|> pure ()
  many1 (jsonPathElement <?> "jsonPathElement")

jsonPathElement :: Parser JSONPathElement
jsonPathElement =
  do
    keyChildDot <?> "keyChildDot"
    <|> (keyChildBracket <?> "keyChildBracket")
    <|> (anyChild <?> "anyChild")
    <|> (slice <?> "slice")
    <|> (sliceUnion <?> "sliceUnion")
    <|> (filterParser <?> "filterParser")
    <|> (search <?> "search")
    <|> (searchBeginningWithSlice <?> "searchBeginningWithSlice")

slice :: Parser JSONPathElement
slice = Slice <$> ignoreSurroundingSqBr sliceWithoutBrackets

sliceWithoutBrackets :: Parser SliceElement
sliceWithoutBrackets =
  (multipleIndices <?> "multipleIndices")
    <|> (singleIndex <?> "singleIndex")

singleIndex :: Parser SliceElement
singleIndex = SingleIndex <$> signed decimal

multipleIndices :: Parser SliceElement
multipleIndices = do
  MultipleIndices
    <$> parseStart
    <*> parseEnd
    <*> parseStep
  where
    parseStart =
      optional (signed decimal)
        <* char ':'

    parseEnd = optional (signed decimal)

    parseStep =
      optional (char ':')
        *> optional (signed decimal)

keyChildBracket :: Parser JSONPathElement
keyChildBracket =
  ignoreSurroundingSqBr $
    ignoreSurroundingSpace $
      KeyChild <$> quotedString

keyChildDot :: Parser JSONPathElement
keyChildDot =
  KeyChild
    <$> (char '.' *> takeWhile1 (inClass "a-zA-Z0-9_-"))

anyChild :: Parser JSONPathElement
anyChild = AnyChild <$ (string ".*" <|> string "[*]")

sliceUnion :: Parser JSONPathElement
sliceUnion = ignoreSurroundingSqBr $ do
  firstElement <- sliceWithoutBrackets <?> "firstElement"
  _ <- char ','
  secondElement <- sliceWithoutBrackets <?> "secondElement"
  return $ SliceUnion firstElement secondElement

filterParser :: Parser JSONPathElement
filterParser = do
  _ <- string "[?(" <?> "[?("
  b <- beginningPoint <?> "beginning point"
  js <- jsonPath <?> "jsonPathElements"
  c <- condition <?> "condition"
  l <- literal <?> "literal"
  _ <- string ")]" <?> ")]"
  return $ Filter b js c l

search :: Parser JSONPathElement
search = do
  _ <- char '.'
  isDot <- (== '.') <$> peekChar'
  if isDot
    then Search <$> many1 jsonPathElement
    else fail "not a search element"

searchBeginningWithSlice :: Parser JSONPathElement
searchBeginningWithSlice = do
  _ <- string ".."
  isBracket <- (== '[') <$> peekChar'
  if isBracket
    then Search <$> many1 jsonPathElement
    else fail "not a search element"

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

ignoreSurroundingSpace :: Parser a -> Parser a
ignoreSurroundingSpace p = many' space *> p <* many' space

ignoreSurroundingSqBr :: Parser a -> Parser a
ignoreSurroundingSqBr p = char '[' *> p <* char ']'

quotedString :: Parser Text
quotedString = inQuotes '"' <|> inQuotes '\''
  where
    inQuotes quoteChar =
      char quoteChar *> A.takeWhile (/= quoteChar) <* char quoteChar
