module Data.JSONPath.Execute
  (executeJSONPath, executeJSONPathEither, executeJSONPathElement)
where

import Data.Aeson
import Data.Aeson.Text
import Data.Function       ((&))
import Data.HashMap.Strict as Map
import Data.JSONPath.Types
import Data.Text           (unpack)

import qualified Data.Text.Lazy as LazyText
import qualified Data.Vector    as V

executeJSONPath :: [JSONPathElement] -> Value -> ExecutionResult Value
executeJSONPath [] val = ResultError "empty json path"
executeJSONPath (j:[]) val = executeJSONPathElement j val
executeJSONPath (j:js) val = executeJSONPath js =<< executeJSONPathElement j val

executeJSONPathEither :: [JSONPathElement] -> Value -> Either String [Value]
executeJSONPathEither js val = resultToEither $ executeJSONPath js val

executeJSONPathElement :: JSONPathElement -> Value -> ExecutionResult Value
executeJSONPathElement (KeyChild key) val =
  case val of
    Object o -> Map.lookup key o
                & (maybeToResult (notFoundErr key o))
    _ -> ResultError $ expectedObjectErr val
executeJSONPathElement (AnyChild) val =
  case val of
    Object o -> ResultList $ Map.elems o
    Array a  -> ResultList $ V.toList a
    _        -> ResultError $ expectedObjectErr val
executeJSONPathElement (Slice slice) val =
  case val of
    Array a -> executeSliceElement slice a
    _       -> ResultError $ expectedArrayErr val
executeJSONPathElement (SliceUnion first second) val =
  case val of
    Array a -> appendResults (executeSliceElement first a) (executeSliceElement second a)
    _ -> ResultError $ expectedArrayErr val
executeJSONPathElement (Filter _ jsonPath cond lit) val =
  case val of
    Array a -> do
      let l = V.toList a
      ResultList $ Prelude.map (executeJSONPath jsonPath) l
        & zip l
        & excludeSndErrors
        & Prelude.foldr (\(x,ys) acc -> if length ys == 1 then (x, head ys):acc else acc) []
        & Prelude.filter (\(origVal, exprVal) -> executeCondition exprVal cond lit)
        & Prelude.map fst
    _ -> ResultError $ expectedArrayErr val
executeJSONPathElement s@(Search js) val =
  let x = either (const []) id $ executeJSONPathEither js val
      y = excludeErrors $ valMap (executeJSONPathElement s) val
  in if Prelude.null x && Prelude.null y
     then ResultError "Search failed"
     else ResultList $ x ++ y

valMap :: ToJSON b => (Value -> ExecutionResult b) -> Value -> [ExecutionResult b]
valMap f v@(Object o) = elems $ Map.map f o
valMap f (Array a) = V.toList $ V.map f a
valMap _ v = pure $ ResultError $ "Expected object or array, found " <> (encodeJSONToString v)

executeCondition :: Value -> Condition -> Literal -> Bool
executeCondition (Number n1) Equal (LitNumber n2) = n1 == (fromInteger $ toInteger n2)
executeCondition (String s1) Equal (LitString s2) = s1 == s2

executeSliceElement :: SliceElement -> V.Vector Value -> ExecutionResult Value
executeSliceElement (SingleIndex i) v                = if i < 0
                                                          then maybeToResult (invalidIndexErr i v) $ (V.!?) v (V.length v + i)
                                                          else maybeToResult (invalidIndexErr i v) $ (V.!?) v i
executeSliceElement (SimpleSlice start end) v        = sliceEither v start end 1
executeSliceElement (SliceWithStep start end step) v = sliceEither v start end step
executeSliceElement (SliceTo end) v                  = sliceEither v 0 end 1
executeSliceElement (SliceToWithStep end step) v     = sliceEither v 0 end step
executeSliceElement (SliceFrom start) v              = sliceEither v start (-1) 1
executeSliceElement (SliceFromWithStep start step) v = sliceEither v start (-1) step
executeSliceElement (SliceWithOnlyStep step) v       = sliceEither v 0 (-1) step

sliceEither :: ToJSON a
    => V.Vector a -> Int -> Int -> Int -> ExecutionResult a
sliceEither v start end step = let len = V.length v
                                   realStart = if start < 0 then len + start else start
                                   realEnd = if end < 0 then len + end + 1 else end
                               in if realStart < realEnd
                                  then appendResults (indexEither v realStart) (sliceEither v (realStart + step) realEnd step)
                                  else ResultList []

indexEither :: ToJSON a => V.Vector a -> Int -> ExecutionResult a
indexEither v i = (V.!?) v i
                  & maybeToResult (invalidIndexErr i v)

excludeSndErrors :: [(c, ExecutionResult a)] -> [(c, [a])]
excludeSndErrors xs = Prelude.foldr accumulateFn ([] :: [(c, b)]) xs where
  accumulateFn (x, ResultList ys) acc = (x, ys):acc
  accumulateFn (x, ResultValue y) acc = (x, [y]):acc
  accumulateFn (x, _) acc             = acc

encodeJSONToString :: ToJSON a => a -> String
encodeJSONToString x = LazyText.unpack $ encodeToLazyText x

notFoundErr key o = "expected key " <> unpack key <> " in object " <> (encodeJSONToString o)
invalidIndexErr i a = "index " <> show i <> " invalid for array " <> (encodeJSONToString a)
expectedObjectErr val = "expected object, found " <> (encodeJSONToString val)
expectedArrayErr val = "expected array, found " <> (encodeJSONToString val)
