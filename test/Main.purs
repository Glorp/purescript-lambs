module Test.Main where

import Prelude (Unit, discard, (<>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (Maybe(..))

import Lambs.Parse (parseTop)
import Lambs.Eval (Exec(..), stepExec)
import Lambs.Unparse (unparse)
import Lambs.Toplevel (Toplevel(..))

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "You could add some tests."
  log (testStep "λa.(λx.λy.x) y")
  log (testStep "λa.λb.(λx.λy.x) y")
  log (testStep "λf.λx.(λa.λb.λf.f a b) (f x) x")
  log (testStep "(λx.λy.x) y")

testStep :: String -> String
testStep s =
  case parseTop s of
    Just (Trm t) ->
      case stepExec t of
        Rename _ _ _ t2 -> "   " <> s <> "\n=> " <> unparse t2 <> "\n\n"
        _ -> ":(("
    _ -> ":("
