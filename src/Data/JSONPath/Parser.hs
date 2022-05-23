{-# LANGUAGE OverloadedStrings #-}
module Data.JSONPath.Parser
  (jsonPathElement, jsonPath)
where

import Control.Applicative  ((<|>))
import Data.Attoparsec.Text as A
import Data.Functor
import Data.JSONPath.Types

jsonPath :: Parser [JSONPathElement]
jsonPath = do
  _ <- skip (== '$') <|> pure ()
  many1 jsonPathElement

jsonPathElement :: Parser JSONPathElement
jsonPathElement = do
    (keyChildDot <?> "keyChildDot")
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
sliceWithoutBrackets = (sliceWithStep <?> "sliceWithStep")
                       <|> (simpleSlice <?> "simpleSlice")
                       <|> (sliceFromWithStep <?> "sliceFromWithStep")
                       <|> (sliceFrom <?> "sliceFrom")
                       <|> (singleIndex <?> "singleIndex")
                       <|> (sliceToWithStep <?> "sliceToWithStep")
                       <|> (sliceTo <?> "sliceTo")
                       <|> (sliceWithOnlyStep <?> "sliceWithOnlyStep")

singleIndex :: Parser SliceElement
singleIndex = SingleIndex <$> signed decimal

keyChildBracket :: Parser JSONPathElement
keyChildBracket =
  ignoreSurroundingSqBr $
  ignoreSurroundingSpace $
  fmap KeyChild $
  char '\'' *> takeWhile1 (/= '\'') <* char '\''

keyChildDot :: Parser JSONPathElement
keyChildDot = KeyChild
              <$> (char '.' *> takeWhile1 (inClass "a-zA-Z0-9_-"))


anyChild :: Parser JSONPathElement
anyChild = AnyChild <$ (string ".*" <|> string "[*]")

simpleSlice :: Parser SliceElement
simpleSlice = do
  start <- signed decimal
  _ <- char ':'
  end <- signed decimal
  return $ SimpleSlice start end

sliceWithStep :: Parser SliceElement
sliceWithStep = do
  start <- signed decimal
  _ <- char ':'
  end <- signed decimal
  _ <- char ':'
  step <- signed decimal
  return $ SliceWithStep start end step

sliceFrom :: Parser SliceElement
sliceFrom = do
  start <- signed decimal
  _ <- char ':'
  return $ SliceFrom start

sliceFromWithStep :: Parser SliceElement
sliceFromWithStep = do
  start <- signed decimal
  _ <- string "::"
  step <- signed decimal
  return $ SliceFromWithStep start step

sliceTo :: Parser SliceElement
sliceTo = do
  _ <- char ':'
  end <- signed decimal
  return $ SliceTo end

sliceToWithStep :: Parser SliceElement
sliceToWithStep = do
  _ <- char ':'
  end <- signed decimal
  _ <- char ':'
  step <- signed decimal
  return $ SliceToWithStep end step

sliceWithOnlyStep :: Parser SliceElement
sliceWithOnlyStep = do
  _ <- string "::"
  step <- signed decimal
  return $ SliceWithOnlyStep step

sliceUnion :: Parser JSONPathElement
sliceUnion = ignoreSurroundingSqBr $  do
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
    else  fail "not a search element"

searchBeginningWithSlice :: Parser JSONPathElement
searchBeginningWithSlice = do
  _ <- string ".."
  isBracket <- (== '[') <$> peekChar'
  if isBracket
    then Search <$> many1 jsonPathElement
    else  fail "not a search element"

beginningPoint :: Parser BeginningPoint
beginningPoint = do
  ((char '$' $> Root) <|> (char '@' $> CurrentObject))

condition :: Parser Condition
condition = ignoreSurroundingSpace
            $ string "==" $> Equal
            <|> string "!=" $> NotEqual
            <|> string "<=" $> SmallerThanOrEqual
            <|> string ">=" $> GreaterThanOrEqual
            <|> string ">" $> GreaterThan
            <|> string "<" $> SmallerThan

literal :: Parser Literal
literal = do
  (LitNumber <$> double)
  <|> LitString <$> (char '"' *> A.takeWhile (/= '"') <* char '"')

ignoreSurroundingSpace :: Parser a -> Parser a
ignoreSurroundingSpace p = many' space *> p <* many' space

ignoreSurroundingSqBr :: Parser a -> Parser a
ignoreSurroundingSqBr p = char '[' *> p <* char ']'
