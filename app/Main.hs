module Main where

import           Assignment
import           BoolFm     (BinOp (And, Eq), BoolFm (B, T, V))
import           Compiler

main :: IO ()
main = do
  print $ constPropNorFm . boolFmToNorFm $ B Eq (V 'a') T
  print And
