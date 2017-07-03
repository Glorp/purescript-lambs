module Lambs.Term (Term(..), Found(..), TermPath, findTerm, fillTerm, lamStep, funStep, argStep, emptyTermPath) where

import Prelude (class Eq, class Ord, class Show, Unit, flip, unit)
import Data.Generic (class Generic, gCompare, gEq, gShow)
import Data.List (List(..), foldl, (:))
import Data.Maybe (Maybe(..))

-- | A `Term` is one of:
-- |  * a `Var`iable with a name
-- |  * a `Lam`bda abstraction with a parameter name and a body `Term`
-- |  * a function `App`lication with a function `Term` and an arugment `Term`
data Term
  = Var String
  | Lam String Term
  | App Term Term

derive instance genericTerm :: Generic Term
instance showTerm :: Show Term where show = gShow
instance eqTerm :: Eq Term where eq = gEq
instance orTerm :: Ord Term where compare = gCompare

-- | Kind of a path into a term, focusing on something kind of...
data TermPath = TermPath (List (Term -> Term))

-- | If you looked for something in a `Term`, this like,
-- |  1. where you found it and
-- |  2. what you found
data Found a = Found TermPath a

-- | For building `TermPath` with a `Lam` in it.
lamStep :: String -> TermPath -> TermPath
lamStep p (TermPath l) = TermPath (Lam p : l)

-- | For building `TermPath` with an `App` in it.
-- | (Step into function part.)
funStep :: Term -> TermPath -> TermPath
funStep a (TermPath l) = TermPath ((flip App) a : l)

-- | For building `TermPath` with another `App` in it.
-- | (Step into argument part.)
argStep :: Term -> TermPath -> TermPath
argStep f (TermPath l) = TermPath (App f : l)

-- | A `TermPath` that doesn't have a ton of stuff in it.s
emptyTermPath :: TermPath
emptyTermPath = TermPath Nil

orElse :: forall a. Maybe a -> (Unit -> Maybe a) -> Maybe a
orElse Nothing f = f unit
orElse x _ = x

-- | For looking for stuff in a `Term`. Maybe you'll find.
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


-- | So uh a `TermPath` is kind of like a `Term` except it's missing a `Term`.
-- | So if you have a `Term` for it, you can get a `Term`.
-- |
-- | `Term`. `Term` `Term` `Term`.
fillTerm :: TermPath -> Term -> Term
fillTerm (TermPath l) term = foldl (\t f -> f t) term l
