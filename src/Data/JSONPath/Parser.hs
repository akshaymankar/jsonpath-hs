{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.JSONPath.Parser (jsonPathElement, jsonPath) where

import qualified Data.Char as Char
import Data.Functor
import Data.Functor.Identity
import Data.JSONPath.Types
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Text.Megaparsec as P
import Text.Megaparsec.Char (char, digitChar, space, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = P.ParsecT Void Text Identity

jsonPath :: Parser a -> Parser [JSONPathElement]
jsonPath endParser = do
  _ <- optional $ char '$'
  manyTill jsonPathElement (hidden $ lookAhead endParser)

jsonPathElement :: Parser JSONPathElement
jsonPathElement =
  ignoreSurroundingSpace $
    try anyChild
      <|> try keyChild
      <|> try slice
      <|> try indexChild
      <|> try union
      <|> try filterParser
      <|> try search
      <|> searchBeginningWithSlice

indexChild :: Parser JSONPathElement
indexChild = IndexChild <$> inSqBr indexChildWithoutBrackets

indexChildWithoutBrackets :: Parser Int
indexChildWithoutBrackets = ignoreSurroundingSpace $ L.signed space boundedDecimal

slice :: Parser JSONPathElement
slice =
  uncurry3 Slice
    <$> inSqBr sliceWithoutBrackets

sliceWithoutBrackets :: Parser (Maybe Int, Maybe Int, Maybe Int)
sliceWithoutBrackets = do
  (,,)
    <$> parseStart
    <*> parseEnd
    <*> parseStep
  where
    parseStart :: Parser (Maybe Int)
    parseStart =
      ignoreSurroundingSpace (optional (L.signed space boundedDecimal))
        <* char ':'

    parseEnd =
      ignoreSurroundingSpace $ optional $ L.signed space boundedDecimal

    parseStep =
      optional (char ':')
        *> ignoreSurroundingSpace (optional (L.signed space boundedDecimal))

keyChild :: Parser JSONPathElement
keyChild = KeyChild <$> (try sqBrKeyChild <|> dotKeyChild)

sqBrKeyChild :: Parser Text
sqBrKeyChild =
  inSqBr $ ignoreSurroundingSpace quotedString

dotKeyChild :: Parser Text
dotKeyChild = do
  _ <- char '.'
  let firstChar c = Char.isAlpha c || c == '_' || not (Char.isAscii c)
      restChar c = Char.isNumber c || firstChar c
  Text.cons
    <$> satisfy firstChar
    <*> takeWhileP Nothing restChar

anyChild :: Parser JSONPathElement
anyChild = ignoreSurroundingSpace $ AnyChild <$ (void (string ".*") <|> void (inSqBr (char '*')))

union :: Parser JSONPathElement
union =
  inSqBr $
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
filterParser = inSqBr $ do
  _ <- ignoreSurroundingSpace $ char '?'
  Filter <$> filterExpr (ignoreSurroundingSpace (char ']'))

filterExpr :: Parser a -> Parser FilterExpr
filterExpr endParser =
  try (orFilterExpr endParser)
    <|> try (andFilterExpr endParser)
    <|> basicFilterExpr endParser

basicFilterExpr :: Parser a -> Parser FilterExpr
basicFilterExpr endParser = do
  maybeNot <- optional (char '!')
  expr <-
    try (comparisionFilterExpr endParser)
      <|> try (existsFilterExpr endParser)
      <|> (inParens (filterExpr closingParen) <* lookAhead endParser)
  case maybeNot of
    Nothing -> pure expr
    Just _ -> pure $ Not expr

comparisionFilterExpr :: Parser a -> Parser FilterExpr
comparisionFilterExpr endParser = do
  expr <-
    ComparisonExpr
      <$> comparable condition
      <*> condition
      <*> comparable endParser
  _ <- lookAhead endParser
  pure expr

existsFilterExpr :: Parser a -> Parser FilterExpr
existsFilterExpr endParser =
  ExistsExpr <$> singularPath endParser

singularPath :: Parser a -> Parser SingularPath
singularPath endParser =
  SingularPath
    <$> beginningPoint
    <*> manyTill singularPathElement (lookAhead endParser)

singularPathElement :: Parser SingularPathElement
singularPathElement =
  (Key <$> try dotKeyChild)
    <|> (Key <$> try sqBrKeyChild)
    <|> Index <$> inSqBr indexChildWithoutBrackets

orFilterExpr :: Parser a -> Parser FilterExpr
orFilterExpr endParser = do
  let orOperator = ignoreSurroundingSpace $ string "||"
  e1 <-
    -- If there is an '&&' operation, it should take precedence over the '||'
    try (andFilterExpr orOperator)
      <|> basicFilterExpr orOperator
  _ <- orOperator
  Or e1 <$> filterExpr endParser

andFilterExpr :: Parser a -> Parser FilterExpr
andFilterExpr endParser = do
  let andOperator = ignoreSurroundingSpace $ string "&&"
  e1 <- basicFilterExpr andOperator
  _ <- andOperator
  And e1 <$> filterExpr endParser

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
beginningPoint =
  try (char '$' $> Root)
    <|> (char '@' $> CurrentObject)

condition :: Parser Condition
condition =
  ignoreSurroundingSpace $
    string "==" $> Equal
      <|> string "!=" $> NotEqual
      <|> string "<=" $> SmallerThanOrEqual
      <|> string ">=" $> GreaterThanOrEqual
      <|> string ">" $> GreaterThan
      <|> string "<" $> SmallerThan

comparable :: Parser a -> Parser Comparable
comparable endParser = do
  CmpNumber <$> L.scientific
    <|> CmpString <$> quotedString
    <|> CmpBool <$> bool
    <|> CmpNull <$ string "null"
    <|> CmpPath <$> singularPath endParser

bool :: Parser Bool
bool =
  True <$ string "true"
    <|> False <$ string "false"

ignoreSurroundingSpace :: Parser a -> Parser a
ignoreSurroundingSpace p = space *> p <* space

inSqBr :: Parser a -> Parser a
inSqBr p = openingSqBr *> p <* closingSqBr

openingSqBr :: Parser Char
openingSqBr = ignoreSurroundingSpace (char '[')

closingSqBr :: Parser Char
closingSqBr = ignoreSurroundingSpace (char ']')

inParens :: Parser a -> Parser a
inParens p = openingParen *> p <* closingParen

openingParen :: Parser Char
openingParen = ignoreSurroundingSpace (char '(')

closingParen :: Parser Char
closingParen = ignoreSurroundingSpace (char ')')

quotedString :: Parser Text
quotedString = ignoreSurroundingSpace $ Text.pack <$> (inQuotes '"' <|> inQuotes '\'')
  where
    inQuotes quoteChar =
      char quoteChar *> manyTill (charLiteral quoteChar) (char quoteChar)

charLiteral :: Char -> Parser Char
charLiteral q =
  unescaped
    <|> (char '\\' *> (char q <|> escapable))
    <|> (if q == '\'' then char '"' else char '\'')

unescaped :: Parser Char
unescaped = satisfy $ \c -> c >= ' ' && c /= '\'' && c /= '"' && c /= '\\'

escapable :: Parser Char
escapable = bs <|> ff <|> lf <|> cr <|> ht <|> slash <|> backslash <|> hexchar
  where
    bs = char 'b' $> '\BS'
    ff = char 'f' $> '\FF'
    lf = char 'n' $> '\LF'
    cr = char 'r' $> '\CR'
    ht = char 't' $> '\HT'
    slash = char '/'
    backslash = char '\\'

hexchar :: Parser Char
hexchar = do
  _ <- char 'u'
  h0 <- hexDigit
  chr <-
    if h0 /= 'D'
      then twoMore h0 =<< hexDigit
      else do
        h1 <- digitChar <|> a <|> b
        if Char.isOctDigit h1
          then twoMore h0 h1
          else surrogate h0 h1
  pure $ Char.chr chr
  where
    surrogate h0 h1 = do
      high <- twoMore h0 h1
      _ <- string "\\u"
      d' <- d
      cdef <- c <|> d <|> e <|> f
      low <- twoMore d' cdef
      pure $ 0x10000 + ((high - 0xD800) * 0x400) + (low - 0xDC00)
    twoMore h0 h1 = do
      h2 <- hexDigit
      h3 <- hexDigit
      pure . read @Int $ "0x" <> [h0, h1, h2, h3]
    hexDigit :: Parser Char =
      digitChar <|> a <|> b <|> c <|> d <|> e <|> f
    a = char 'A' <|> Char.toUpper <$> char 'a'
    b = char 'B' <|> Char.toUpper <$> char 'b'
    c = char 'C' <|> Char.toUpper <$> char 'c'
    d = char 'D' <|> Char.toUpper <$> char 'd'
    e = char 'E' <|> Char.toUpper <$> char 'e'
    f = char 'F' <|> Char.toUpper <$> char 'f'

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

-- | __WARNING__: This fails to parse the 'minBound' when used with signed
-- integers
boundedDecimal :: forall a. (Bounded a, Integral a) => Parser a
boundedDecimal = (<?> "bounded integer") $ do
  firstDigit <- digitChar
  let firstInt = toInteger $ Char.digitToInt firstDigit
  go (toInteger (maxBound @a)) firstInt [firstDigit]
  where
    go :: Integer -> Integer -> [Char] -> Parser a
    go limit acc readDigits = do
      mNextDigit <- optional $ lookAhead digitChar
      case mNextDigit of
        Nothing -> pure $ fromInteger acc
        Just nextDigit -> do
          let acc' = acc * 10 + toInteger (Char.digitToInt nextDigit)
          if acc' > limit
            then unexpected (Label $ NonEmpty.fromList "integer overflow")
            else anySingle *> go limit acc' (readDigits <> [nextDigit])
