module Purpl (evalSync, jsonparse, Context) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception as Ex
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, runEffFn1, runEffFn2)
import Data.Either (Either(..))
import Data.Maybe (Maybe, maybe)

foreign import data Context ∷ Type
foreign import data Script ∷ Type → Type
foreign import data Require ∷ Type

foreign import require ∷ Require
foreign import jsonparse ∷ ∀ a. String → a
foreign import createContextImpl ∷ ∀ eff r. EffFn1 eff { | r} Context
foreign import mkScriptImpl ∷ ∀ a eff. EffFn1 eff String (Script a)
foreign import runInContextImpl ∷ ∀ a eff. EffFn2 eff Context (Script a) a

mkScript ∷ ∀ e a. String → Eff e (Script a)
mkScript = runEffFn1 mkScriptImpl

runInContext ∷ ∀ e a. Context → Script a → Eff e a
runInContext = runEffFn2 runInContextImpl

shimRequire ∷ String → String
shimRequire s = "(function(require){ return (" <> s <> ") })"

evalSync ∷ ∀ eff. String → Maybe Context → Eff eff { context ∷ Context, result ∷ String }
evalSync code mCtx = do
  ctx ← maybe (runEffFn1 createContextImpl {}) pure mCtx
  script ∷ Script (Require → String) ← mkScript (shimRequire code)
  result ← Ex.try $ (_ $ require) <$> runInContext ctx script
  case result of
    Right r → pure { context: ctx, result: r }
    Left err → pure { context: ctx, result: Ex.message err }
