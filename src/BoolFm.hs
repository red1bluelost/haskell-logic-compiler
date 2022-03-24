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
boolFmp (V v)     = varp v
boolFmp (U _ p)   = boolFmp p
boolFmp (B _ p q) = boolFmp p && boolFmp q
boolFmp _         = True

boolFm1p :: Recognizer BoolFm
boolFm1p (B Nand p q) = boolFm1p p && boolFm1p q
boolFm1p f            = boolFmp f

norFmp :: Recognizer BoolFm
norFmp (B Nor p q) = norFmp p && norFmp q
norFmp B {}        = False
norFmp U {}        = False
norFmp f           = boolFmp f

norNCFmp :: Recognizer BoolFm
norNCFmp T   = False
norNCFmp Nil = False
norNCFmp f   = norFmp f

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
