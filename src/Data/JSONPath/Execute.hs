{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.JSONPath.Execute (executeJSONPath, executeJSONPathElement) where

import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as Map
import qualified Data.Foldable as Foldable
import Data.JSONPath.Types
import Data.Maybe (fromMaybe, isJust, maybeToList)
import qualified Data.Vector as V

executeJSONPath :: [JSONPathElement] -> Value -> [Value]
executeJSONPath [] v = [v]
executeJSONPath (j : js) v = executeJSONPath js =<< executeJSONPathElement j v

executeJSONPathElement :: JSONPathElement -> Value -> [Value]
executeJSONPathElement (KeyChild key) val =
  maybeToList $ executeSingularPathElement (Key key) val
executeJSONPathElement AnyChild val =
  case val of
    Object o -> map snd $ Map.toList o
    Array a -> V.toList a
    _ -> []
executeJSONPathElement (IndexChild i) val =
  maybeToList $ executeSingularPathElement (Index i) val
executeJSONPathElement (Slice start end step) val =
  case val of
    Array a -> executeSlice start end step a
    _ -> []
executeJSONPathElement (Union elements) val =
  concatMap (flip executeUnionElement val) elements
executeJSONPathElement (Filter expr) val =
  case val of
    Array a -> executeFilter expr (V.toList a)
    Object o -> executeFilter expr (Map.elems o)
    _ -> []
executeJSONPathElement s@(Search js) val =
  let x = executeJSONPath js val
      y = mconcat $ valMap (executeJSONPathElement s) val
   in x <> y

valMap :: ToJSON b => (Value -> [b]) -> Value -> [[b]]
valMap f (Object o) = map snd . Map.toList $ Map.map f o
valMap f (Array a) = V.toList $ V.map f a
valMap _ _ = []

executeConditionOnMaybes :: Maybe Value -> Condition -> Maybe Value -> Bool
executeConditionOnMaybes (Just val1) c (Just val2) = executeCondition val1 c val2
executeConditionOnMaybes Nothing Equal Nothing = True
executeConditionOnMaybes Nothing GreaterThanOrEqual Nothing = True
executeConditionOnMaybes Nothing SmallerThanOrEqual Nothing = True
executeConditionOnMaybes Nothing NotEqual (Just _) = True
executeConditionOnMaybes (Just _) NotEqual Nothing = True
executeConditionOnMaybes _ _ _ = False

{- ORMOLU_DISABLE -}
isEqualTo :: Value -> Value -> Bool
(Object _) `isEqualTo` _          = False
_          `isEqualTo` (Object _) = False
(Array _)  `isEqualTo` _          = False
_          `isEqualTo` (Array _)  = False
val1       `isEqualTo` val2       = val1 == val2

isSmallerThan :: Value -> Value -> Bool
(Number n1) `isSmallerThan` (Number n2) = n1 < n2
(String s1) `isSmallerThan` (String s2) = s1 < s2
_           `isSmallerThan` _ = False
{- ORMOLU_ENABLE -}

executeCondition :: Value -> Condition -> Value -> Bool
executeCondition val1 NotEqual val2 = not (executeCondition val1 Equal val2)
executeCondition val1 Equal val2 = val1 `isEqualTo` val2
executeCondition val1 SmallerThan val2 = val1 `isSmallerThan` val2
executeCondition val1 GreaterThan val2 =
  canCompare val1 val2
    && not (executeCondition val1 SmallerThan val2)
    && not (executeCondition val1 Equal val2)
executeCondition val GreaterThanOrEqual lit =
  canCompare val lit
    && not (executeCondition val SmallerThan lit)
executeCondition val1 SmallerThanOrEqual val2 =
  canCompare val1 val2
    && not (executeCondition val1 GreaterThan val2)

canCompare :: Value -> Value -> Bool
canCompare (Number _) (Number _) = True
canCompare (String _) (String _) = True
canCompare _ _ = False

-- | Implementation is based on
-- https://ietf-wg-jsonpath.github.io/draft-ietf-jsonpath-base/draft-ietf-jsonpath-base.html#name-array-slice-selector
executeSlice :: forall a. Maybe Int -> Maybe Int -> Maybe Int -> V.Vector a -> [a]
executeSlice mStart mEnd mStep v
  | step == 0 = []
  | step > 0 = postitiveStepLoop lowerBound
  | otherwise = negativeStepLoop upperBound
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

executeIndexChild :: Int -> V.Vector a -> Maybe a
executeIndexChild i v =
  if i < 0
    then (V.!?) v (V.length v + i)
    else (V.!?) v i

executeUnionElement :: UnionElement -> Value -> [Value]
executeUnionElement (UEIndexChild i) v = executeJSONPathElement (IndexChild i) v
executeUnionElement (UESlice start end step) v = executeJSONPathElement (Slice start end step) v
executeUnionElement (UEKeyChild child) v = executeJSONPathElement (KeyChild child) v

executeSingularPathElement :: SingularPathElement -> Value -> Maybe Value
executeSingularPathElement (Key key) val =
  case val of
    Object o -> Map.lookup (Key.fromText key) o
    _ -> Nothing
executeSingularPathElement (Index i) val =
  case val of
    Array a -> executeIndexChild i a
    _ -> Nothing

executeSingularPath :: SingularPath -> Value -> Maybe Value
executeSingularPath (SingularPath _ ps) val =
  Foldable.foldl'
    ( \case
        Nothing -> const Nothing
        Just v -> flip executeSingularPathElement v
    )
    (Just val)
    ps

executeFilter :: FilterExpr -> [Value] -> [Value]
executeFilter expr = Prelude.filter (filterExprPred expr)

comparableToValue :: Comparable -> Value -> Maybe Value
comparableToValue (CmpNumber n) _ = Just $ Number n
comparableToValue (CmpString s) _ = Just $ String s
comparableToValue (CmpBool b) _ = Just $ Bool b
comparableToValue CmpNull _ = Just Null
comparableToValue (CmpPath p) val = executeSingularPath p val

filterExprPred :: FilterExpr -> Value -> Bool
filterExprPred (ComparisonExpr cmp1 cond cmp2) val =
  let val1 = comparableToValue cmp1 val
      val2 = comparableToValue cmp2 val
   in executeConditionOnMaybes val1 cond val2
filterExprPred (ExistsExpr path) val = isJust $ executeSingularPath path val
filterExprPred (Or e1 e2) val = filterExprPred e1 val || filterExprPred e2 val
filterExprPred (And e1 e2) val = filterExprPred e1 val && filterExprPred e2 val
filterExprPred (Not e) val = not $ filterExprPred e val
