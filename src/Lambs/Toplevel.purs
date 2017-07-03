module Lambs.Toplevel (Toplevel(..)) where

import Lambs.Term (Term)
import Lambs.Define (Define, Undefine)
import Prelude (class Eq, class Ord, class Show)
import Data.Generic (class Generic, gCompare, gEq, gShow)

-- | `Toplevel` things are for toplevel things. Can be:
-- |  * `Def`initions, for adding toplevel defintions
-- |  * `Undef`initions, for removing toplevel defintions
-- |  * `Trm`s, with `Term`s in them. The `Term`s can be evaluated (is fun)
data Toplevel
  = Def Define
  | Undef Undefine
  | Trm Term

derive instance genericToplevel :: Generic Toplevel
instance showToplevel :: Show Toplevel where show = gShow
instance eqToplevel :: Eq Toplevel where eq = gEq
instance orToplevel :: Ord Toplevel where compare = gCompare
