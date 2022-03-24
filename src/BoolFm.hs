module BoolFm
  ( UnOp (..),
    BinOp (..),
    BoolFm (..),
    boolFmp,
    boolFm1p,
    norFmp,
    norNCFmp,
    norCPFmp,
    bfEval,
  )
where

import           Assignment          (Assignment, Var, assignmentp, lookup)
import           Contract.Input      (checkInput2)
import           Contract.Recognizer (Recognizer)
import           Operation.Binary    (BinOp (..), bop)
import           Operation.Unary     (UnOp (..), uop)

data BoolFm
  = T
  | Nil
  | V Var
  | U UnOp BoolFm
  | B BinOp BoolFm BoolFm
  deriving (Eq)

boolFmp :: Recognizer BoolFm
boolFmp _ = True

boolFm1p :: Recognizer BoolFm
boolFm1p (B Nand _ _) = False
boolFm1p _            = True

norFmp :: Recognizer BoolFm
norFmp (B Nor _ _) = True
norFmp B {}        = False
norFmp U {}        = False
norFmp _           = True

norNCFmp :: Recognizer BoolFm
norNCFmp (B Nor _ _) = True
norNCFmp B {}        = False
norNCFmp U {}        = False
norNCFmp T           = False
norNCFmp Nil         = False
norNCFmp _           = True

norCPFmp :: Recognizer BoolFm
norCPFmp T   = True
norCPFmp Nil = True
norCPFmp f   = norNCFmp f

instance Show BoolFm where
  show T         = "t"
  show Nil       = "nil"
  show (V v)     = [v]
  show (U o p)   = "(" ++ show o ++ " " ++ show p ++ ")"
  show (B o p q) = "(" ++ show p ++ " " ++ show o ++ " " ++ show p ++ ")"

bfEval :: BoolFm -> Assignment -> Bool
bfEval f a = checkInput2 bfEvalImpl f [boolFmp] a [assignmentp]

bfEvalImpl :: BoolFm -> Assignment -> Bool
bfEvalImpl T _         = True
bfEvalImpl Nil _       = False
bfEvalImpl (V v) a     = Assignment.lookup v a
bfEvalImpl (U o p) a   = uop o (bfEvalImpl p a)
bfEvalImpl (B o p q) a = bop o (bfEvalImpl p a) (bfEvalImpl q a)
