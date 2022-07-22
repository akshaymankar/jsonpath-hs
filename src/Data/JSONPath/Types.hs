module Data.JSONPath.Types
  ( BeginningPoint (..),
    Condition (..),
    Comparable (..),
    JSONPathElement (..),
    UnionElement (..),
    FilterExpr (..),
    SingularPathElement (..),
    SingularPath (..),
  )
where

import Data.Scientific (Scientific)
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

data Comparable
  = CmpNumber Scientific
  | CmpString Text
  | CmpBool Bool
  | CmpNull
  | CmpPath SingularPath
  deriving (Show, Eq)

data SingularPathElement
  = Key Text
  | Index Int
  deriving (Show, Eq)

data SingularPath
  = SingularPath BeginningPoint [SingularPathElement]
  deriving (Show, Eq)

data FilterExpr
  = ExistsExpr SingularPath
  | ComparisonExpr Comparable Condition Comparable
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
