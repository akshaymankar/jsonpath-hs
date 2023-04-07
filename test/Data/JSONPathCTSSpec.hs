{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.JSONPathCTSSpec where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.TH
import Data.Aeson.Text
import Data.Bifunctor (Bifunctor (first))
import qualified Data.ByteString.Lazy as LBS
import Data.Either
import Data.FileEmbed
import Data.JSONPath
import Data.Text (Text, unpack)
import qualified Data.Text.Lazy as LazyText
import qualified Data.Vector as V
import GHC.Generics
import System.Timeout
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

data Test = Test
  { name :: Text,
    selector :: Text,
    document :: Maybe Value,
    result :: Maybe Value,
    invalid_selector :: Maybe Bool
  }
  deriving (Eq, Show, Generic)

data CTS = CTS
  { tests :: [Test]
  }
  deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''Test)
$(deriveJSON defaultOptions ''CTS)

spec :: Spec
spec =
  let testFiles = $(embedFile "test/resources/jsonpath-compliance-test-suite/cts.json")
      testVals :: Either String [CTS]
      testVals = traverse (eitherDecode . LBS.fromStrict) [testFiles]
   in case testVals of
        Left e ->
          describe "JSONPath Compliance Test Suite" $
            it "shouldn't fail to parse test files" $
              expectationFailure ("failed to parse test files with error: \n" <> e)
        Right gs -> describe "JSONPath" $
          do
            mapM_ group gs
            describe "Parser" $ do
              it "should parse basic things" $ do
                parse (jsonPathElement <* eof) "" ".foo"
                  `shouldParse` KeyChild "foo"
                parse (jsonPath eof) "" "$.foo"
                  `shouldParse` [KeyChild "foo"]

parseJSONPath :: Text -> Either String [JSONPathElement]
parseJSONPath = first errorBundlePretty . parse (jsonPath eof) ""

group :: CTS -> Spec
group CTS {..} = do
  describe "CTS" $
    mapM_ test tests

-- | 100 ms
timeLimit :: Int
timeLimit = 100000

test :: Test -> Spec
test (Test name path (Just testData) (Just expected) _) =
  it (unpack path) $ do
    mResult <-
      liftIO $
        timeout timeLimit $ do
          -- Using '$!' here ensures that the computation is strict, so this can
          -- be timed out properly
          pure $! do
            parsed <- parseJSONPath path
            Right $ executeJSONPath parsed testData

    result <- case mResult of
      Just r -> pure r
      Nothing -> do
        expectationFailure "JSONPath execution timed out"
        undefined

    case expected of
      Array a -> case result of
        Left e -> expectationFailure $ "Unexpected Left: " <> e
        -- TODO: Define order of result and make this `shouldBe`
        Right r -> r `shouldMatchList` V.toList a
      Bool False -> result `shouldSatisfy` isLeft
      v -> expectationFailure $ "Invalid result in test data " <> LazyText.unpack (encodeToLazyText v)
test (Test name path _ _ _) =
  it (unpack path) $ do
    mResult <-
      liftIO $
        timeout timeLimit $ do
          -- Using '$!' here ensures that the computation is strict, so this can
          -- be timed out properly
          pure $! do
            parsed <- parseJSONPath path
            -- TODO: no need to run the query, but need to figure out what to return instead
            Right $ executeJSONPath parsed Null

    result <- case mResult of
      Just r -> pure r
      Nothing -> do
        expectationFailure "JSONPath execution timed out"
        undefined

    result `shouldSatisfy` isLeft
