module Lambs.Term (Term(..), Found(..), TermPath, findTerm, fillTerm, lamStep, funStep, argStep, emptyTermPath) where

import Prelude (class Eq, class Ord, class Show, Unit, flip, unit)
import Data.Generic (class Generic, gCompare, gEq, gShow)
import Data.List (List(..), foldl, (:))
import Data.Maybe (Maybe(..))

data Term
  = Var String
  | Lam String Term
  | App Term Term

derive instance genericTerm :: Generic Term
instance showTerm :: Show Term where show = gShow
instance eqTerm :: Eq Term where eq = gEq
instance orTerm :: Ord Term where compare = gCompare

data TermPath = TermPath (List (Term -> Term))

data Found a = Found TermPath a

lamStep :: String -> TermPath -> TermPath
lamStep p (TermPath l) = TermPath (Lam p : l)

funStep :: Term -> TermPath -> TermPath
funStep a (TermPath l) = TermPath ((flip App) a : l)

argStep :: Term -> TermPath -> TermPath
argStep f (TermPath l) = TermPath (App f : l)

emptyTermPath :: TermPath
emptyTermPath = TermPath Nil

orElse :: forall a. Maybe a -> (Unit -> Maybe a) -> Maybe a
orElse Nothing f = f unit
orElse x _ = x

findTerm :: forall a. (Term -> Maybe a) -> Term -> Maybe (Found a)
findTerm pred term = find emptyTermPath term
  where
    find :: TermPath -> Term -> Maybe (Found a)
    find tp t =
      case pred t of
        Nothing -> next t
        Just x -> Just (Found tp x)
      where
        next :: Term -> Maybe (Found a)
        next (Var _) = Nothing
        next (Lam p x) = find (lamStep p tp) x
        next (App f a) =
          orElse (find (funStep a tp) f) (\_ -> find (argStep f tp) a)

fillTerm :: TermPath -> Term -> Term
fillTerm (TermPath l) term = foldl (\t f -> f t) term l
