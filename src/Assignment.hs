module Assignment (Var, Assignment, Assignment.lookup, varp, assignmentp) where

import           Contract.Input      (checkInput2)
import           Contract.Recognizer (Recognizer)
import           Data.Char           (isLower)

type Var = Char

varp :: Recognizer Var
varp = isLower

type Assignment = [Var]

assignmentp :: Recognizer Assignment
assignmentp = all varp

lookup :: Var -> Assignment -> Bool
lookup v a = checkInput2 elem v [varp] a [assignmentp]
