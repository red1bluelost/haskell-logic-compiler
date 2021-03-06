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

import           Assignment          (Assignment, Var, assignmentp, lookup,
                                      varp)
import           Contract.Input      (checkInput2)
import           Contract.Output     (checkOutput)
import           Contract.Recognizer (Recognizer)
import           Data.Default        (def)
import           Data.Either         (fromRight)
import           Operation.Binary    (BinOp (..), bop)
import           Operation.Unary     (UnOp (..), uop)
import           Text.Megaparsec     (parse)
import           Text.SExpression    (SExpr (Atom, List), def, parseSExpr)

data BoolFm
  = T
  | Nil
  | V Var
  | U UnOp BoolFm
  | B BinOp BoolFm BoolFm
  deriving (Eq)

boolFmp :: Recognizer BoolFm
boolFmp T         = True
boolFmp Nil       = True
boolFmp (V v)     = varp v
boolFmp (U _ p)   = boolFmp p
boolFmp (B _ p q) = boolFmp p && boolFmp q

boolFm1p :: Recognizer BoolFm
boolFm1p (B Nand _ _) = False
boolFm1p T            = True
boolFm1p Nil          = True
boolFm1p (V v)        = varp v
boolFm1p (U _ p)      = boolFm1p p
boolFm1p (B _ p q)    = boolFm1p p && boolFm1p q

norFmp :: Recognizer BoolFm
norFmp T           = True
norFmp Nil         = True
norFmp (V v)       = varp v
norFmp U {}        = False
norFmp (B Nor p q) = norFmp p && norFmp q
norFmp B {}        = False

norNCFmp :: Recognizer BoolFm
norNCFmp T           = False
norNCFmp Nil         = False
norNCFmp (V v)       = varp v
norNCFmp U {}        = False
norNCFmp (B Nor p q) = norNCFmp p && norNCFmp q
norNCFmp B {}        = False

norCPFmp :: Recognizer BoolFm
norCPFmp T   = True
norCPFmp Nil = True
norCPFmp f   = norNCFmp f

instance Show BoolFm where
  show T         = "t"
  show Nil       = "nil"
  show (V v)     = [v]
  show (U o p)   = "(" ++ show o ++ " " ++ show p ++ ")"
  show (B o p q) = "(" ++ show p ++ " " ++ show o ++ " " ++ show q ++ ")"

instance Read BoolFm where
  readsPrec _ s = readBoolFm s

readBoolFm :: ReadS BoolFm
readBoolFm s = case parse (parseSExpr def) "" s of
  Left _ -> []
  Right sexpr ->
    case sexprToBoolFm sexpr of
      Nothing -> []
      Just f  -> [(f, "") | boolFmp f]

sexprToBoolFm :: SExpr -> Maybe BoolFm
sexprToBoolFm (Atom "t") = Just T
sexprToBoolFm (Atom "nil") = Just Nil
sexprToBoolFm (Atom [c]) = if varp c then Just $ V c else Nothing
sexprToBoolFm (List [Atom os, p]) =
  case sexprToBoolFm p of
    Just pt -> Just $ U (read os :: UnOp) pt
    Nothing -> Nothing
sexprToBoolFm (List [p, Atom os, q]) =
  case (sexprToBoolFm p, sexprToBoolFm q) of
    (Just pt, Just qt) -> Just $ B (read os :: BinOp) pt qt
    _                  -> Nothing
sexprToBoolFm s = Nothing

bfEval :: BoolFm -> Assignment -> Bool
bfEval f a = checkInput2 bfEvalImpl f [boolFmp] a [assignmentp]

bfEvalImpl :: BoolFm -> Assignment -> Bool
bfEvalImpl T _         = True
bfEvalImpl Nil _       = False
bfEvalImpl (V v) a     = Assignment.lookup v a
bfEvalImpl (U o p) a   = uop o (bfEvalImpl p a)
bfEvalImpl (B o p q) a = bop o (bfEvalImpl p a) (bfEvalImpl q a)
