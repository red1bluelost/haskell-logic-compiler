module Compiler (boolFmToNorFm, constPropNorFm) where

import           BoolFm
import           Contract.Input
import           Contract.Output (checkOutput)

boolFmToNorFm :: BoolFm -> BoolFm
boolFmToNorFm = flip checkOutput [norFmp]

boolFmToNorFmImpl :: BoolFm -> BoolFm
boolFmToNorFmImpl T = T
boolFmToNorFmImpl Nil = Nil
boolFmToNorFmImpl (V v) = V v
boolFmToNorFmImpl (U Not p) =
  let pt = boolFmToNorFmImpl p
   in B Nor pt pt
boolFmToNorFmImpl (B Nor p q) =
  let pt = boolFmToNorFmImpl p
      qt = boolFmToNorFmImpl q
   in B Nor pt qt
boolFmToNorFmImpl (B And p q) =
  boolFmToNorFmImpl $ B Nor (U Not p) (U Not q)
boolFmToNorFmImpl (B Or p q) =
  boolFmToNorFmImpl $ U Not (B Nor p q)
boolFmToNorFmImpl (B Imp p q) =
  boolFmToNorFmImpl $ B Or (U Not p) q
boolFmToNorFmImpl (B Eq p q) =
  boolFmToNorFmImpl $ B And (B Imp p q) (B Imp q p)
boolFmToNorFmImpl (B Xor p q) =
  boolFmToNorFmImpl $ B Eq (U Not p) q
boolFmToNorFmImpl (B Nand p q) =
  boolFmToNorFmImpl $ U Not (B And p q)

constPropNorFm :: BoolFm -> BoolFm
constPropNorFm f =
  let r = checkInput1 constPropNorFmImpl f [norFmp]
   in checkOutput r [norCPFmp]

constPropNorFmImpl :: BoolFm -> BoolFm
constPropNorFmImpl (B Nor p q) =
  let np = constPropNorFmImpl p
      nq = constPropNorFmImpl q
   in f (np, nq)
  where
    f (T, _)     = Nil
    f (_, T)     = Nil
    f (Nil, Nil) = T
    f (Nil, nq)  = B Nor nq nq
    f (np, Nil)  = B Nor np np
    f (np, nq)   = B Nor np nq
constPropNorFmImpl p = p
