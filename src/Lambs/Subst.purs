module Lambs.Subst (Redex(..), redex, subst) where

import Lambs.Term (Term(Var, Lam, App))

import Data.Generic (class Generic, gCompare, gEq, gShow)
import Data.Maybe (Maybe(..))
import Prelude (class Eq, class Ord, class Show, (==))

data Redex = Redex Term String Term
derive instance genericRedex :: Generic Redex
instance showRedex :: Show Redex where show = gShow
instance eqRedex :: Eq Redex where eq = gEq
instance orRedex :: Ord Redex where compare = gCompare

redex :: Term -> Maybe Redex
redex (App (Lam p b) a) = Just (Redex a p b)
redex _ = Nothing

subst :: Redex -> Term
subst (Redex arg param body) = halp body
  where
    halp :: Term -> Term
    halp (App f a) = App (halp f) (halp a)
    halp (Lam p b) = Lam p (if p == param then b else halp b)
    halp (Var s) = if s == param then arg else (Var s)
