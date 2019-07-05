module Data.JSONPath.Types
  ( BegingingPoint(..)
  , Condition(..)
  , Literal(..)
  , JSONPathElement(..)
  , SliceElement(..)
  , module Data.JSONPath.ExecutionResult
  )
where

import Data.Text
import Data.JSONPath.ExecutionResult

data BegingingPoint = Root
                    | CurrentObject
  deriving (Show, Eq)

data Condition = Equal
               | NotEqual
               | GreaterThan
               | SmallerThan
  deriving (Show, Eq)

data Literal = LitNumber Int
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
                      | KeyChildren [Text]
                      | AnyChild
                      | Slice SliceElement
                      | SliceUnion SliceElement SliceElement
                      | Filter BegingingPoint [JSONPathElement] Condition Literal
                      | Search [JSONPathElement]
  deriving (Show, Eq)
