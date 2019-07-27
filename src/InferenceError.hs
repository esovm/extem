module InferenceError
  ( ErrMsg
  , InferenceError(..)
  , FactInferenceError
  , buildInferenceError
  ) where

import ESTypes (AtomicFact)

type ErrMsg = String
data InferenceError a = InferenceError a ErrMsg deriving Show
type FactInferenceError = InferenceError AtomicFact

buildInferenceError :: Show a => a -> ErrMsg -> InferenceError a
buildInferenceError v m = InferenceError v ("Unable to infer " ++ show v ++ ": " ++ m)
