module Data.JSONPath.Types
  ( BeginningPoint(..)
  , Condition(..)
  , Literal(..)
  , JSONPathElement(..)
  , SliceElement(..)
  , module Data.JSONPath.ExecutionResult
  )
where

import Data.Text
import Data.JSONPath.ExecutionResult

data BeginningPoint = Root
                    | CurrentObject
  deriving (Show, Eq)

data Condition = Equal
               | NotEqual
               | GreaterThan
               | SmallerThan
               | GreaterEqualThan
               | SmallerEqualThan
  deriving (Show, Eq)

data Literal = LitNumber Double
             | LitString Text
  deriving (Show, Eq)

data SliceElement = SingleIndex Int
                  | SimpleSlice Int Int
                  | SliceWithStep Int Int Int
                  | SliceTo Int
                  | SliceToWithStep Int Int
                  | SliceFrom Int
                  | SliceFromWithStep Int Int
                  | SliceWithOnlyStep Int
  deriving (Show, Eq)

data JSONPathElement  = KeyChild Text
                      | AnyChild
                      | Slice SliceElement
                      | SliceUnion SliceElement SliceElement
                      | Filter BeginningPoint [JSONPathElement] Condition Literal
                      | Search [JSONPathElement]
  deriving (Show, Eq)
