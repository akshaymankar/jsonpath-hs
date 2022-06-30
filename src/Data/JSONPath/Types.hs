module Data.JSONPath.Types
  ( BeginningPoint (..),
    Condition (..),
    Literal (..),
    JSONPathElement (..),
    UnionElement (..),
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

data UnionElement
  = UEKeyChild Text
  | UEIndexChild Int
  | UESlice (Maybe Int) (Maybe Int) (Maybe Int)
  deriving (Show, Eq)

data JSONPathElement
  = KeyChild Text
  | IndexChild Int
  | AnyChild
  | Slice (Maybe Int) (Maybe Int) (Maybe Int)
  | Union [UnionElement]
  | Filter BeginningPoint [JSONPathElement] Condition Literal
  | Search [JSONPathElement]
  deriving (Show, Eq)
