module Data.JSONPath.Types
  ( BeginningPoint (..),
    Condition (..),
    Literal (..),
    JSONPathElement (..),
    SliceElement (..),
  )
where

import Data.Text

data BeginningPoint
  = Root
  | CurrentObject
  deriving (Show, Eq)

data Condition
  = Equal
  | NotEqual
  | GreaterThan
  | SmallerThan
  | GreaterThanOrEqual
  | SmallerThanOrEqual
  deriving (Show, Eq)

data Literal
  = LitNumber Double
  | LitString Text
  deriving (Show, Eq)

data SliceElement
  = SingleIndex Int
  | MultipleIndices (Maybe Int) (Maybe Int) (Maybe Int)
  deriving (Show, Eq)

data JSONPathElement
  = KeyChild Text
  | AnyChild
  | Slice SliceElement
  | SliceUnion SliceElement SliceElement
  | Filter BeginningPoint [JSONPathElement] Condition Literal
  | Search [JSONPathElement]
  deriving (Show, Eq)
