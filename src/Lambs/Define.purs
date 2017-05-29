module Lambs.Define
  (Define(..), Undefine(..), Definitions, substDefs, noDefinitions, definitionsList, addDef, removeDef)
  where

import Lambs.Subst (Redex(..), subst)
import Lambs.Term (Term)

import Data.List (List(..), (:))
import Data.Foldable (foldr)
import Prelude (class Eq, class Ord, class Show, (==))
import Data.Generic (class Generic, gCompare, gEq, gShow)

data Define = Define String Term

derive instance genericDefine :: Generic Define
instance showDefine :: Show Define where show = gShow
instance eqDefine :: Eq Define where eq = gEq
instance orDefine :: Ord Define where compare = gCompare

data Undefine = Undefine String

derive instance genericUndefine :: Generic Undefine
instance showUndefine :: Show Undefine where show = gShow
instance eqUndefine :: Eq Undefine where eq = gEq
instance orUndefine :: Ord Undefine where compare = gCompare


data Definitions = Definitions (List Define)

derive instance genericDefinitions :: Generic Definitions
instance showDefinitions :: Show Definitions where show = gShow
instance eqDefinitions :: Eq Definitions where eq = gEq
instance orDefinitions :: Ord Definitions where compare = gCompare

substDefs :: Definitions -> Term -> Term
substDefs (Definitions l) term = foldr substDef term l
  where substDef (Define name def) t = subst (Redex def name t)

noDefinitions :: Definitions
noDefinitions = Definitions Nil

addDef :: Define -> Definitions -> Definitions
addDef (Define name def) (Definitions l) = Definitions (addTo l)
  where
    addTo Nil = Define name def : Nil
    addTo (Define n d : t) =
      if name == n
      then Define name def : t
      else Define n d : addTo t

removeDef :: Undefine -> Definitions -> Definitions
removeDef (Undefine name) (Definitions l) = Definitions (removeFrom l)
  where
    removeFrom Nil = Nil
    removeFrom (Define n d : t) =
      if name == n
      then t
      else Define n d : removeFrom t

definitionsList :: Definitions -> List Define
definitionsList (Definitions l) = l
