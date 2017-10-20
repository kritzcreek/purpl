module Purpl (evalSync, createContext, jsonparse, Context) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Uncurried (EffFn2, runEffFn2)
import Data.Maybe (Maybe(..))

foreign import data Context ∷ Type

foreign import jsonparse ∷ ∀ a. String → a
foreign import purplEval ∷ ∀ eff. EffFn2 eff String Context { context ∷ Context, result ∷ String }
foreign import createContext ∷ ∀ eff r. { | r} → Eff eff Context

evalSync ∷ ∀ eff. String → Maybe Context → Eff eff { context ∷ Context, result ∷ String }
evalSync code = case _ of
  Nothing → do
    runEffFn2 purplEval code =<< createContext {}
  Just ctx →
    runEffFn2 purplEval code ctx
