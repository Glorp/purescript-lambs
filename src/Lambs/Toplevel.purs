module Lambs.Toplevel (Toplevel(..)) where

import Lambs.Term (Term)
import Lambs.Define (Define, Undefine)
import Prelude (class Eq, class Ord, class Show)
import Data.Generic (class Generic, gCompare, gEq, gShow)

data Toplevel
  = Def Define
  | Undef Undefine
  | Trm Term

derive instance genericToplevel :: Generic Toplevel
instance showToplevel :: Show Toplevel where show = gShow
instance eqToplevel :: Eq Toplevel where eq = gEq
instance orToplevel :: Ord Toplevel where compare = gCompare
