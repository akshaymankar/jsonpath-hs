module Data.JSONPath.ExecutionResult where

data ExecutionResult a = ResultList [a]
                       | ResultValue a
                       | ResultError String

instance Functor ExecutionResult where
  fmap f (ResultList xs)   = ResultList $ Prelude.map f xs
  fmap f (ResultValue x)   = ResultValue $ f x
  fmap _ (ResultError err) = ResultError err

instance Applicative ExecutionResult where
  pure = ResultValue
  (<*>) (ResultList fs) (ResultList xs) = ResultList $ fs <*> xs
  (<*>) (ResultList fs) (ResultValue x) = ResultList $ Prelude.map (\f -> f x) fs
  (<*>) (ResultValue f) (ResultList xs) = ResultList $ Prelude.map f xs
  (<*>) (ResultValue f) (ResultValue x) = ResultValue $ f x
  (<*>) (ResultError e) _               = ResultError e
  (<*>) _ (ResultError e)               = ResultError e

instance Monad ExecutionResult where
  (>>=) (ResultValue x) f = f x
  (>>=) (ResultList xs) f = concatResults $ Prelude.map f xs
  (>>=) (ResultError e) _ = ResultError e

concatResults :: [ExecutionResult a] -> ExecutionResult a
concatResults [] = ResultList []
concatResults (ResultList xs:rs) = case concatResults rs of
                                     ResultList ys -> ResultList (xs ++ ys)
                                     ResultValue y -> ResultList (y:xs)
                                     e             -> e
concatResults (ResultValue x:[]) = ResultValue x
concatResults (ResultValue x:rs) = case concatResults rs of
                                     ResultList ys -> ResultList (x:ys)
                                     ResultValue y -> ResultList [x,y]
                                     e             -> e
concatResults (e:_) = e

appendResults :: ExecutionResult a -> ExecutionResult a -> ExecutionResult a
appendResults (ResultValue x) (ResultValue y) = ResultList [x,y]
appendResults (ResultValue x) (ResultList ys) = ResultList $ x:ys
appendResults (ResultList xs) (ResultValue y) = ResultList $ y:xs
appendResults (ResultList xs) (ResultList ys) = ResultList $ xs ++ ys
appendResults _ e                             = e

maybeToResult :: String -> Maybe a ->ExecutionResult a
maybeToResult _ (Just x) = ResultValue x
maybeToResult err _      = ResultError err

resultToEither :: ExecutionResult a -> Either String [a]
resultToEither (ResultList xs) = return xs
resultToEither (ResultValue x) = return [x]
resultToEither (ResultError e) = Left e

excludeErrors :: [ExecutionResult a] -> [a]
excludeErrors []                 = []
excludeErrors (ResultError _:rs) = excludeErrors rs
excludeErrors (ResultList xs:rs) = xs ++ excludeErrors rs
excludeErrors (ResultValue x:rs) = x:(excludeErrors rs)
