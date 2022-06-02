{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.JSONPath.Execute (executeJSONPath, executeJSONPathEither, executeJSONPathElement) where

import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as Map
import Data.Aeson.Text
import Data.Function ((&))
import Data.JSONPath.Types
import Data.Text (Text, unpack)

#if !MIN_VERSION_base (4,11,0)
import Data.Semigroup ((<>))
#endif

import Data.Either (fromRight)
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as LazyText
import qualified Data.Vector as V

executeJSONPath :: [JSONPathElement] -> Value -> ExecutionResult Value
executeJSONPath [] _ = ResultError "empty json path"
executeJSONPath [j] val = executeJSONPathElement j val
executeJSONPath (j : js) val = executeJSONPath js =<< executeJSONPathElement j val

executeJSONPathEither :: [JSONPathElement] -> Value -> Either String [Value]
executeJSONPathEither js val = resultToEither $ executeJSONPath js val

executeJSONPathElement :: JSONPathElement -> Value -> ExecutionResult Value
executeJSONPathElement (KeyChild key) val =
  case val of
    Object o ->
      Map.lookup (Key.fromText key) o
        & maybeToResult (notFoundErr key o)
    _ -> ResultError $ expectedObjectErr val
executeJSONPathElement AnyChild val =
  case val of
    Object o -> ResultList . map snd $ Map.toList o
    Array a -> ResultList $ V.toList a
    _ -> ResultError $ expectedObjectErr val
executeJSONPathElement (Slice slice) val =
  case val of
    Array a -> executeSliceElement slice a
    _ -> ResultError $ expectedArrayErr val
executeJSONPathElement (SliceUnion first second) val =
  case val of
    Array a -> appendResults (executeSliceElement first a) (executeSliceElement second a)
    _ -> ResultError $ expectedArrayErr val
executeJSONPathElement (Filter _ jsonPath cond lit) val =
  case val of
    Array a -> do
      let l = V.toList a
      ResultList $
        Prelude.map (executeJSONPath jsonPath) l
          & zip l
          & excludeSndErrors
          & Prelude.foldr (\(x, ys) acc -> if length ys == 1 then (x, head ys) : acc else acc) []
          & Prelude.filter (\(_, exprVal) -> executeCondition exprVal cond lit)
          & Prelude.map fst
    _ -> ResultError $ expectedArrayErr val
executeJSONPathElement s@(Search js) val =
  let x = fromRight [] $ executeJSONPathEither js val
      y = excludeErrors $ valMap (executeJSONPathElement s) val
   in if Prelude.null x && Prelude.null y
        then ResultError "Search failed"
        else ResultList $ x ++ y

valMap :: ToJSON b => (Value -> ExecutionResult b) -> Value -> [ExecutionResult b]
valMap f (Object o) = map snd . Map.toList $ Map.map f o
valMap f (Array a) = V.toList $ V.map f a
valMap _ v = pure $ ResultError $ "Expected object or array, found " <> encodeJSONToString v

executeCondition :: Value -> Condition -> Literal -> Bool
executeCondition val NotEqual lit = not $ executeCondition val Equal lit
executeCondition (Number n1) Equal (LitNumber n2) = n1 == realToFrac n2
executeCondition (String s1) Equal (LitString s2) = s1 == s2
executeCondition _ Equal _ = False
executeCondition val GreaterThan lit =
  not (executeCondition val SmallerThan lit) && not (executeCondition val Equal lit)
executeCondition val GreaterThanOrEqual lit = not (executeCondition val SmallerThan lit)
executeCondition (Number n1) SmallerThan (LitNumber n2) = n1 < realToFrac n2
executeCondition (String s1) SmallerThan (LitString s2) = s1 < s2
executeCondition _ SmallerThan _ = False
executeCondition val SmallerThanOrEqual lit = not (executeCondition val GreaterThan lit)

executeSliceElement :: SliceElement -> V.Vector Value -> ExecutionResult Value
executeSliceElement (SingleIndex i) v =
  if i < 0
    then maybeToResult (invalidIndexErr i v) $ (V.!?) v (V.length v + i)
    else maybeToResult (invalidIndexErr i v) $ (V.!?) v i
executeSliceElement (SimpleSlice start end) v = sliceEither v (Just start) (Just end) Nothing
executeSliceElement (SliceWithStep start end step) v = sliceEither v (Just start) (Just end) (Just step)
executeSliceElement (SliceTo end) v = sliceEither v Nothing (Just end) Nothing
executeSliceElement (SliceToWithStep end step) v = sliceEither v Nothing (Just end) (Just step)
executeSliceElement (SliceFrom start) v = sliceEither v (Just start) Nothing Nothing
executeSliceElement (SliceFromWithStep start step) v = sliceEither v (Just start) Nothing (Just step)
executeSliceElement (SliceWithOnlyStep step) v = sliceEither v Nothing Nothing (Just step)

-- | Based on
-- https://ietf-wg-jsonpath.github.io/draft-ietf-jsonpath-base/draft-ietf-jsonpath-base.html#name-array-slice-selector
sliceEither ::
  forall a.
  ToJSON a =>
  V.Vector a ->
  Maybe Int ->
  Maybe Int ->
  Maybe Int ->
  ExecutionResult a
sliceEither v mStart mEnd mStep
  | step == 0 = ResultList []
  | step > 0 = ResultList $ postitiveStepLoop lowerBound
  | otherwise = ResultList $ negativeStepLoop upperBound
  where
    postitiveStepLoop :: Int -> [a]
    postitiveStepLoop i
      | i < upperBound = v V.! i : postitiveStepLoop (i + step)
      | otherwise = []

    negativeStepLoop :: Int -> [a]
    negativeStepLoop i
      | i > lowerBound = v V.! i : negativeStepLoop (i + step)
      | otherwise = []

    normalizeIndex :: Int -> Int
    normalizeIndex i =
      if i >= 0 then i else len + i

    len = V.length v
    step = fromMaybe 1 mStep

    defaultStart
      | step >= 0 = 0
      | otherwise = len - 1
    start = fromMaybe defaultStart mStart
    normalizedStart = normalizeIndex start

    defaultEnd
      | step >= 0 = len
      | otherwise = negate len - 1
    end = fromMaybe defaultEnd mEnd
    normalizedEnd = normalizeIndex end

    lowerBound
      | step >= 0 = min (max normalizedStart 0) len
      | otherwise = min (max normalizedEnd (-1)) (len - 1)
    upperBound
      | step >= 0 = min (max normalizedEnd 0) len
      | otherwise = min (max normalizedStart (-1)) (len - 1)

excludeSndErrors :: [(c, ExecutionResult a)] -> [(c, [a])]
excludeSndErrors = Prelude.foldr accumulateFn ([] :: [(c, b)])
  where
    accumulateFn (x, ResultList ys) acc = (x, ys) : acc
    accumulateFn (x, ResultValue y) acc = (x, [y]) : acc
    accumulateFn _ acc = acc

encodeJSONToString :: ToJSON a => a -> String
encodeJSONToString x = LazyText.unpack $ encodeToLazyText x

notFoundErr :: ToJSON a => Text -> a -> String
notFoundErr key o = "expected key " <> unpack key <> " in object " <> encodeJSONToString o

invalidIndexErr :: (ToJSON a) => Int -> a -> String
invalidIndexErr i a = "index " <> show i <> " invalid for array " <> encodeJSONToString a

expectedObjectErr :: ToJSON a => a -> String
expectedObjectErr val = "expected object, found " <> encodeJSONToString val

expectedArrayErr :: ToJSON a => a -> String
expectedArrayErr val = "expected array, found " <> encodeJSONToString val
