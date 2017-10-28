module Main where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Exception as Ex
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Node.Process (stdin)
import Node.ReadLine (READLINE, close, createInterface, prompt, setLineHandler)
import Purpl (eval, jsonparse)

wrapSynchJS ∷ String → String
wrapSynchJS expr = """
function(err, succ) {
  try {
""" <> "succ(" <> expr <> ")" <>
"""
  } catch (e) {
  err(e)
}
}
"""

main :: ∀ eff. Eff (readline ∷ READLINE, console ∷ CONSOLE, exception ∷ EXCEPTION | eff) Unit
main = do
  interface ← createInterface stdin mempty
  prompt interface
  setLineHandler interface \s →
    if s == "quit"
      then close interface
      else do
       eval Nothing (wrapSynchJS (jsonparse s)) (either (log <<< Ex.message) log)
