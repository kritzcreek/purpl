module Purpl (eval, jsonparse, createContext, store, Context) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Exception as Ex
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, mkEffFn1, runEffFn1, runEffFn2)
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Data.Maybe (Maybe(..), maybe)

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

createContext :: forall e r. { | r } -> Eff e Context
createContext = runEffFn1 createContextImpl

shimRequire ∷ String → String
shimRequire s = "(function(require){ return (" <> s <> ") })"

declareVariable :: String -> String
declareVariable ident = "let " <> ident <> ";"

storeAs :: forall eff. String -> String
storeAs ident = "(function(v) { " <> ident <> " = v })"

-- | The type of the JavaScript we expect to be sent for evaluation
type JSEval eff r = EffFn2 eff (EffFn1 eff Error Unit) (EffFn1 eff r Unit) Unit

store :: ∀ eff. Context -> String → String → (Maybe Error → Eff eff Unit) → Eff eff Unit
store ctx ident code cb = do
  script ∷ Script (Require → JSEval eff Foreign) ← mkScript (shimRequire code)
  result ← Ex.try $ (_ $ require) <$> runInContext ctx script
  case result of
    Left err →
      cb (Just err)
    Right r → do
      runEffFn2 r (mkEffFn1 (cb <<< Just)) (mkEffFn1 \v -> storeResult v *> cb Nothing)
  where
    storeResult v = do
      script :: Script (EffFn1 eff Foreign Unit) <- mkScript (storeAs ident)
      _ <- runInContext ctx =<< mkScript (declareVariable ident)
      setter <- runInContext ctx script
      runEffFn1 setter v

eval ∷ ∀ eff. Maybe Context → String → (Either Error String → Eff eff Unit) → Eff eff Unit
eval mCtx code cb = do
  ctx ← maybe (runEffFn1 createContextImpl {}) pure mCtx
  script ∷ Script (Require → JSEval eff String) ← mkScript (shimRequire code)
  result ← Ex.try $ (_ $ require) <$> runInContext ctx script
  case result of
    Left err →
      cb (Left err)
    Right r → do
      runEffFn2 r (mkEffFn1 (cb <<< Left)) (mkEffFn1 (cb <<< Right))
