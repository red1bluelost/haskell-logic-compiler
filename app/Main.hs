module Main where

import           BoolFm   (BoolFm)
import           Compiler (boolFmToNorFm, constPropNorFm)

stf :: String -> BoolFm
stf = read

example :: String -> IO ()
example s = do
  let f = stf s
  putStrLn "Given Formala: "
  print f
  putStrLn "BoolFm -> NorFm -> NorCPFm: "
  print . constPropNorFm . boolFmToNorFm $ f
  putStrLn ""

main :: IO ()
main = do
  mapM_
    example
    [ "t",
      "nil",
      "p",
      "(! p)",
      "(p ^ q)",
      "(p v q)",
      "(p => q)",
      "(p = q)",
      "(p <> q)",
      "(p !^ q)",
      "(p !v q)",
      "(((! p)  => (q v r)) !^ p)"
    ]
