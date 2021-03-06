module Lambs.Eval (Exec(..), stepExec) where

import Data.Set as Set
import Data.Generic (class Generic, gCompare, gEq, gShow)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (charAt, length, splitAt)
import Lambs.Subst (Redex(..), redex, subst)
import Lambs.Term (Found(..), Term(..), TermPath, argStep, emptyTermPath, fillTerm, findTerm, funStep, lamStep)
import Prelude (class Eq, class Ord, class Show, Unit, show, unit, (&&), (+), (/=), (<>), (==), (-), bind)

-- | An `Exec` is used to represent one step of execution. Is one of following:
-- |
-- |  * A `Reduce`, for one step of beta reduction, with:
-- |    1. old `Term`
-- |    2. the `Term` after one step of beta reduction
-- |  * A `Rename`, for the renaming of one variable (one parameter and every
-- |    reference to it). Typically for avoiding variable capture. Has:
-- |    1. old name of variable
-- |    2. old `Term`
-- |    3. new name of variable
-- |    4. the `Term` with variable renamed to new name
-- |  * A `Normal`, for the, ah, determination that a `Term` is on normal form. Has:
-- |    1. the `Term` that is on normal form
data Exec
  = Reduce Term Term
  | Rename String Term String Term
  | Normal Term

derive instance genericExec :: Generic Exec
instance showExec :: Show Exec where show = gShow
instance eqExec :: Eq Exec where eq = gEq
instance ordExec :: Ord Exec where compare = gCompare

freeIds :: Term -> Set.Set String
freeIds t = free t Set.empty
  where
    free :: Term -> Set.Set String -> Set.Set String
    free (Var s) bound =
      if Set.member s bound then Set.empty else Set.singleton s
    free (App f a) bound = Set.union (free f bound) (free a bound)
    free (Lam p b) bound = free b (Set.insert p bound)

allIds :: Term -> Set.Set String
allIds (Var s) = Set.singleton s
allIds (App f a) = Set.union (allIds f) (allIds a)
allIds (Lam p b) = Set.insert p (allIds b)

data Conflict = Conflict String Term

conflict :: String -> Set.Set String -> Term -> Maybe Conflict
conflict param bad (Lam p b) =
  if p /= param && Set.member p bad && Set.member param (freeIds b)
  then Just (Conflict p b)
  else Nothing
conflict _ _ _ = Nothing


data IdNum = IdNum String Int

digits :: Set.Set Char
digits = Set.fromFoldable ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

isDigit :: Char -> Boolean
isDigit c = Set.member c digits

idNum :: String -> IdNum
idNum s =
  case res of
    Just x -> x
    Nothing -> IdNum s 1
  where
    halp i =
      case charAt i s of
        Nothing -> Nothing
        Just c ->
          if isDigit c
          then halp (i - 1)
          else Just (i + 1)
    res =
      do
        i <- halp (length s - 1)
        spl <- splitAt i s
        n <- fromString spl.after
        Just (IdNum spl.before n)

uniqueId :: Set.Set String -> String -> String
uniqueId used str =
  case idNum str of
    IdNum s n -> halp s (n + 1)
  where
    halp s n =
      let newId = s <> show n
      in if Set.member newId used then halp s (n + 1) else newId

data Ren = Ren String String Term
rename :: Term -> Found Conflict -> Ren
rename t (Found tp (Conflict p b)) =
  Ren p newId (fillTerm tp (Lam newId (subst (Redex (Var newId) p b))))
  where newId = uniqueId (allIds t) p

orElse :: forall a. Maybe a -> (Unit -> Maybe a) -> Maybe a
orElse Nothing f = f unit
orElse x _ = x

findConflict :: Set.Set String -> Redex -> Maybe (Found Conflict)
findConflict argFree (Redex arg param body) = find emptyTermPath body
  where
    find :: TermPath -> Term -> Maybe (Found Conflict)
    find tp t =
      case conflict param argFree t of
        Nothing -> next t
        Just x -> Just (Found tp x)
      where
        next :: Term -> Maybe (Found Conflict)
        next (Var _) = Nothing
        next (Lam p x) = if p == param then Nothing else find (lamStep p tp) x
        next (App f a) =
          orElse (find (funStep a tp) f) (\_ -> find (argStep f tp) a)

-- | For performing one step of execution.
stepExec :: Term -> Exec
stepExec term =
  case findTerm redex term of
    Nothing -> Normal term
    Just (Found tp r) -> reduceRename (Found tp r) (findConf r)
  where
    renameHalp :: Found Redex -> Ren -> Exec
    renameHalp (Found tp (Redex arg param body)) (Ren old new t) =
      Rename old term new (fillTerm tp ((App (Lam param t)) arg))

    reduceRename :: Found Redex -> Maybe (Found Conflict) -> Exec
    reduceRename (Found tp r) Nothing = Reduce term (fillTerm tp (subst r))
    reduceRename f (Just c) = renameHalp f (rename term c)

    findConf :: Redex -> Maybe (Found Conflict)
    findConf (Redex a p b) = findConflict (freeIds a) (Redex a p b)
