module Data.JSONPath.Types
  ( BeginningPoint (..),
    Condition (..),
    Comparable (..),
    JSONPathElement (..),
    UnionElement (..),
    FilterExpr (..),
    SingularPathElement (..),
    SingularPath (..),
    FilterQuery(..),
    FunctionExpr (..),
    FunctionArgument (..),
    FunctionName (..)
  )
where

import Data.Scientific (Scientific)
import Data.Text

data BeginningPoint
  = Root
  | CurrentObject
  deriving (Show, Eq)

-- | A JSONPath which finds at max one value, given a beginning point. Used by
-- 'FilterExpr' for 'ExistsExpr' and 'ComparisonExpr'.
data SingularPath
  = SingularPath BeginningPoint [SingularPathElement]
  deriving (Show, Eq)

data SingularPathElement
  = Key Text
  | Index Int
  deriving (Show, Eq)

data FunctionExpr
  = FunctionExpr FunctionName [FunctionArgument]
  deriving (Show, Eq)

data FunctionName
  = FunLength
  | FunCount
  | FunMatch
  | FunSearch
  | FunValue
  deriving (Show, Eq)

data FunctionArgument
  = ArgLiteral Comparable -- except CmpPath or CmpFun
  | ArgFilterQuery FilterQuery
  | ArgLogicalExpr FilterExpr
  | ArgFunctionExpr FunctionExpr
  deriving (Show, Eq)

data Comparable
  = CmpNumber Scientific
  | CmpString Text
  | CmpBool Bool
  | CmpNull
  | CmpPath SingularPath
  | CmpFun FunctionExpr
  deriving (Show, Eq)

data Condition
  = Equal
  | NotEqual
  | GreaterThan
  | SmallerThan
  | GreaterThanOrEqual
  | SmallerThanOrEqual
  deriving (Show, Eq)

data FilterExpr
  = ExistsExpr FilterQuery
  | ExistsFun FunctionExpr
  | ComparisonExpr Comparable Condition Comparable
  | And FilterExpr FilterExpr
  | Or FilterExpr FilterExpr
  | Not FilterExpr
  deriving (Show, Eq)

data FilterQuery = FilterQuery BeginningPoint [JSONPathElement]
  deriving (Show, Eq)

-- | Elements which can occur inside a union
data UnionElement
  = UEKeyChild Text
  | UEIndexChild Int
  | UESlice (Maybe Int) (Maybe Int) (Maybe Int)
  deriving (Show, Eq)

-- | A 'JSONPath' is a list of 'JSONPathElement's.
data JSONPathElement
  = -- | '$.foo' or '$["foo"]'
    KeyChild Text
  | -- | '$[1]'
    IndexChild Int
  | -- | '$[*]'
    AnyChild
  | -- | '$[1:7]', '$[0:10:2]', '$[::2]', '$[::]', etc.
    Slice (Maybe Int) (Maybe Int) (Maybe Int)
  | -- | '$[0,1,9]' or '$[0, 1:2, "foo", "bar"]'
    Union [UnionElement]
  | -- | '$[?(@.foo == 42)]', '$[?(@.foo > @.bar)]', etc.
    Filter FilterExpr
  | -- | '$..foo.bar'
    Search [JSONPathElement]
  deriving (Show, Eq)
