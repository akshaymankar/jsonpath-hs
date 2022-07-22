module Data.JSONPath.Types
  ( BeginningPoint (..),
    Condition (..),
    Literal (..),
    JSONPathElement (..),
    UnionElement (..),
    FilterExpr (..),
    SingularPathElement (..)
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

data SingularPathElement
  = Key Text
  | Index Int
  deriving (Show, Eq)

data FilterExpr
  = ExistsExpr BeginningPoint [SingularPathElement]
  | ComparisonExpr BeginningPoint [SingularPathElement] Condition Literal
  | And FilterExpr FilterExpr
  | Or FilterExpr FilterExpr
  | Not FilterExpr
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
  | Filter FilterExpr
  | Search [JSONPathElement]
  deriving (Show, Eq)
