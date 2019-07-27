module Target where

import ESTypes
import InferenceError
import Data.List (find)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, get, put, gets)
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (MonadIO)

lookupAtomicFact :: AtomicFact -> [AtomicProposition] -> Maybe AtomicProposition
lookupAtomicFact f = find ((== f) . apFact)

resolve :: MonadIO m
        => AtomicFact
        -> StateT ([AtomicFact], KnowledgeBase) m AtomicProposition
resolve fact = undefined
  

resolveQueries :: MonadIO m => [AtomicFact] -> StateT KnowledgeBase m ()
resolveQueries queries = do
  st <- get
  put st
