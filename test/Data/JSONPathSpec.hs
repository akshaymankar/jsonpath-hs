{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Data.JSONPathSpec where

import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.Text
import Data.Aeson.TH
import Data.Attoparsec.Text
import Data.Either
import Data.FileEmbed
import Data.JSONPath
import Data.Text             (Text, unpack)
import GHC.Generics
import Test.Hspec
import Test.Hspec.Attoparsec

#if !MIN_VERSION_base (4,11,0)
import Data.Semigroup ((<>))
#endif

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy       as LazyText
import qualified Data.Vector          as V

data Test = Test { path   :: Text
                 , result :: Value
                 }
            deriving (Eq, Show, Generic)

data TestGroup = TestGroup { groupTitle :: Text
                           , groupData  :: Value
                           , groupTests :: [Test]
                           }
               deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''Test)
$(deriveJSON (aesonPrefix snakeCase) ''TestGroup)

spec :: Spec
spec =
  let testFiles = map snd $(embedDir "test/resources/json-path-tests")
      testVals :: Either String [TestGroup]
      testVals = sequenceA $ map (eitherDecode . LBS.fromStrict) testFiles
  in case testVals of
       Left e -> describe "JSONPath Tests"
                  $ it "shouldn't fail to parse test files"
                  $ expectationFailure ("failed to parse test files with error: \n" <> e)
       Right gs -> describe "JSONPath"
                   $ do
                     mapM_ group gs
                     describe "Parser" $ do
                       it "should parse basic things" $ do
                         (".foo" :: Text) ~> (jsonPathElement <* endOfInput)
                           `shouldParse` KeyChild "foo"
                         ("$.foo" :: Text) ~> (jsonPath <* endOfInput)
                           `shouldParse` [KeyChild "foo"]

parseJSONPath :: Text -> Either String [JSONPathElement]
parseJSONPath = parseOnly (jsonPath <* endOfInput)

group :: TestGroup -> Spec
group TestGroup{..} = describe (unpack groupTitle)
                      $ mapM_ (test groupData) groupTests

test :: Value -> Test -> Spec
test testData (Test path expected) =
  let result = parseJSONPath path >>= (flip executeJSONPathEither testData)
  in it (unpack path) $
     case expected of
       Array a -> case result of
         Left err -> expectationFailure $ "Unexpected Left: " <> err
         Right r  -> r `shouldMatchList` (V.toList a)
       Bool False -> result `shouldSatisfy` isLeft
       v -> expectationFailure $ "Invalid result in test data " <> (LazyText.unpack $ encodeToLazyText v)
