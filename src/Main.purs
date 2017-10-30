module Main where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Exception as Ex
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Newtype (overF)
import Node.Process (stdin)
import Node.ReadLine (READLINE, close, createInterface, prompt, setLineHandler)
import Purpl (createContext, eval, jsonparse, store)

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

example :: forall e. Eff ( console :: CONSOLE | e ) Unit
example = do
  ctx <- createContext {}
  store ctx "myVar" (wrapSynchJS "1 + 1") \_ ->
    eval (Just ctx) (wrapSynchJS "myVar") case _ of
      Left err -> logShow err
      Right v -> log v

{-
main :: ∀ eff. Eff (readline ∷ READLINE, console ∷ CONSOLE, exception ∷ EXCEPTION | eff) Unit
main = do
  interface ← createInterface stdin mempty
  prompt interface
  setLineHandler interface \s →
    if s == "quit"
      then close interface
      else do
       eval Nothing (wrapSynchJS (jsonparse s)) (either (log <<< Ex.message) log)
--}
