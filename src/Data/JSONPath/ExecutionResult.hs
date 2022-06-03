module Data.JSONPath.ExecutionResult where

data ExecutionResult a
  = ResultList [a]
  | ResultValue a
  deriving (Show, Eq)

instance Functor ExecutionResult where
  fmap f (ResultList xs) = ResultList $ map f xs
  fmap f (ResultValue x) = ResultValue $ f x

instance Applicative ExecutionResult where
  pure = ResultValue
  (<*>) (ResultList fs) (ResultList xs) = ResultList $ fs <*> xs
  (<*>) (ResultList fs) (ResultValue x) = ResultList $ map (\f -> f x) fs
  (<*>) (ResultValue f) (ResultList xs) = ResultList $ map f xs
  (<*>) (ResultValue f) (ResultValue x) = ResultValue $ f x

instance Monad ExecutionResult where
  (>>=) (ResultValue x) f = f x
  (>>=) (ResultList xs) f = mconcat $ map f xs

instance Semigroup (ExecutionResult a) where
  ResultValue x <> ResultValue y = ResultList [x, y]
  ResultValue x <> ResultList ys = ResultList $ x : ys
  ResultList xs <> ResultValue y = ResultList $ xs <> [y]
  ResultList xs <> ResultList ys = ResultList $ xs <> ys

instance Monoid (ExecutionResult a) where
  mempty = ResultList []

resultToList :: ExecutionResult a -> [a]
resultToList (ResultList xs) = xs
resultToList (ResultValue x) = [x]
