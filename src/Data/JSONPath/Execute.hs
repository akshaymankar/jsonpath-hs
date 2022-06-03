{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.JSONPath.Execute (executeJSONPath, executeJSONPathElement) where

import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as Map
import Data.Function ((&))
import Data.JSONPath.Types

#if !MIN_VERSION_base (4,11,0)
import Data.Semigroup ((<>))
#endif

import qualified Data.Aeson as A
import Data.Bifunctor (second)
import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Vector as V

executeJSONPath :: [JSONPathElement] -> Value -> ExecutionResult Value
executeJSONPath [] v = ResultValue v
executeJSONPath (j : js) v = executeJSONPath js =<< executeJSONPathElement j v

executeJSONPathElement :: JSONPathElement -> Value -> ExecutionResult Value
executeJSONPathElement (KeyChild key) val =
  case val of
    Object o ->
      maybe (ResultList []) ResultValue $
        Map.lookup (Key.fromText key) o
    _ -> ResultValue A.Null
executeJSONPathElement AnyChild val =
  ResultList $ case val of
    Object o -> map snd $ Map.toList o
    Array a -> V.toList a
    _ -> []
executeJSONPathElement (Slice slice) val =
  case val of
    Array a -> executeSliceElement slice a
    _ -> ResultList []
executeJSONPathElement (SliceUnion firstPart secondPart) val =
  case val of
    Array a -> executeSliceElement firstPart a <> executeSliceElement secondPart a
    _ -> ResultList []
executeJSONPathElement (Filter _ jsonPath cond lit) val =
  case val of
    Array a -> do
      let l = V.toList a
      ResultList $
        map (executeJSONPath jsonPath) l
          & zipWith (curry (second resultToList)) l
          & map
            ( \(x, ys) ->
                -- Use found value, if it exists, otherwise use 'A.Null'. The
                -- null value is helpful if the condition is 'NotEqual'
                if length ys == 1
                  then (x, head ys)
                  else (x, A.Null)
            )
          & Prelude.filter (\(_, exprVal) -> executeCondition exprVal cond lit)
          & Prelude.map fst
    _ -> ResultList []
executeJSONPathElement s@(Search js) val =
  let x = executeJSONPath js val
      y = mconcat $ valMap (executeJSONPathElement s) val
   in dropNulls $ x <> y

valMap :: ToJSON b => (Value -> ExecutionResult b) -> Value -> [ExecutionResult b]
valMap f (Object o) = map snd . Map.toList $ Map.map f o
valMap f (Array a) = V.toList $ V.map f a
valMap _ _ = []

dropNulls :: ExecutionResult Value -> ExecutionResult Value
dropNulls (ResultList xs) = ResultList $ filter (/= A.Null) xs
dropNulls (ResultValue A.Null) = ResultList []
dropNulls (ResultValue x) = ResultList [x]

executeCondition :: Value -> Condition -> Literal -> Bool
executeCondition val NotEqual lit = not (executeCondition val Equal lit)
executeCondition (Number n1) Equal (LitNumber n2) = n1 == realToFrac n2
executeCondition (String s1) Equal (LitString s2) = s1 == s2
executeCondition _ Equal _ = False
executeCondition val GreaterThan lit =
  canCompare val lit
    && not (executeCondition val SmallerThan lit)
    && not (executeCondition val Equal lit)
executeCondition val GreaterThanOrEqual lit =
  canCompare val lit
    && not (executeCondition val SmallerThan lit)
executeCondition (Number n1) SmallerThan (LitNumber n2) = n1 < realToFrac n2
executeCondition (String s1) SmallerThan (LitString s2) = s1 < s2
executeCondition _ SmallerThan _ = False
executeCondition val SmallerThanOrEqual lit =
  canCompare val lit
    && not (executeCondition val GreaterThan lit)

canCompare :: Value -> Literal -> Bool
canCompare (Number _) (LitNumber _) = True
canCompare (String _) (LitString _) = True
canCompare _ _ = False

-- | Implementation of 'MultipleIndices' execution is based on
-- https://ietf-wg-jsonpath.github.io/draft-ietf-jsonpath-base/draft-ietf-jsonpath-base.html#name-array-slice-selector
executeSliceElement :: forall a. ToJSON a => SliceElement -> V.Vector a -> ExecutionResult a
executeSliceElement (SingleIndex i) v
  | i < 0 = ResultList . maybeToList $ (V.!?) v (V.length v + i)
  | otherwise = ResultList . maybeToList $ (V.!?) v i
executeSliceElement (MultipleIndices mStart mEnd mStep) v
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

    len = V.length v
    step = fromMaybe 1 mStep

    normalizeIndex :: Int -> Int
    normalizeIndex i =
      if i >= 0 then i else len + i

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
