module Lambs.Unparse (unparse, unparseDef, unparseDefs) where

import Lambs.Define (Define(..), Definitions, definitionsList)
import Lambs.Term (Term(..))
import Data.Array as Array
import Data.String (joinWith)
import Prelude ((<>), map)

pstring :: String -> String
pstring s = "(" <> s <> ")"

argstring :: Term -> String
argstring (Var s) = s
argstring t = pstring (unparse t)

-- | uns the parse
unparse :: Term -> String
unparse (Lam p b) = "λ" <> p <> "." <> unparse b
unparse (App (Lam p b) a) = pstring (unparse (Lam p b)) <> " " <> argstring a
unparse (App f a) = unparse f <> " " <> argstring a
unparse (Var s) = s

-- | unparse toplevel define-thing
unparseDef :: Define -> String
unparseDef (Define s t) = s <> " ≜ " <> unparse t

-- | like `unparseDef` but for like a `Definitions`ful of them
unparseDefs :: Definitions -> String
unparseDefs ds =
  joinWith "\n" (Array.fromFoldable (map unparseDef (definitionsList ds)))
