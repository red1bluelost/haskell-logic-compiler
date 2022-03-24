module Main where

import qualified Assignment
import           BoolFm     (BoolFm, bfEval, boolFm1p, boolFmp, norCPFmp,
                             norFmp, norNCFmp)
import           Compiler   (boolFmToNorFm, constPropNorFm)

main :: IO ()
main = do
  {-
  Notes on style required differences between ACL2s and Haskell:
    - Haskell requires all types start with an uppercase.
    - Haskell requires all functions start with a lowercase.
    - Haskell has a prelude function called lookup so I have to specify
      where I use it.
  I did use external open source libraries but not for anything required
  in the challenge.
  -}

  {-
  1. My program has one type BoolFm and then recognizers for the required
  subtypes. Having one type with recognizers always for easy conversion
  between subtypes but it requires manually invoking the dynamic type checks.

  An ACL2s definition like:
  (definec f (p :BoolFm1) :NorCPFm ...)
  would translate to Haskell as
  f :: BoolFm -> BoolFm
  f p = let r = checkInput1 ... p [BoolFm1]
         in checkOutput r [NorCPFm]
  The helper functions allow for invocation of recognizers before and
  after the call to a delegate function that implements that actual
  transformations.

  This code can be found in "src/BoolFm.hs" and "src/Contract/*.hs".

  Here are some examples of using the recognizers.
  -}

  let stf :: String -> BoolFm
      stf = read
      f = "((p !v nil) !v (! q))"
  putStrLn "Examples for part 1:"
  print . boolFmp . stf $ f
  print . boolFm1p . stf $ f
  print . norFmp . stf $ f
  print . norNCFmp . stf $ f
  print . norCPFmp . stf $ f
  putStrLn ""

  {-
  2. I created type aliases for `var` and `assignment` as well as
  recognizers. For convenience I required all var's to be lowercase.

  Lookup just checks the input contracts then delegates to the builtin
  `elem` function.

  BfEval checks the input contracts then jumps to the delegate function.
  The `T` and `Nil` values coorespond to Booleans in Haskell, Var performs
  the look up, and finally the operations apply the operation on the Unary
  or Binary formula. This delegates the actual operation to the information
  in the operation.

  This code can be found in "src/BoolFm.hs", "src/Assignment.hs", and
  "src/Operation/*.hs".

  Here are some examples of using lookup and bfEval.
  -}

  putStrLn "Examples for part 2:"
  print $ Assignment.lookup 'a' ['a', 'b', 'c']
  print $ Assignment.lookup 'a' ['x', 'y', 'z']
  print $ bfEval (stf f) ['p']
  print $ bfEval (stf f) ['p', 'q']
  putStrLn ""

  {-
  3. The function constPropNorFmImpl pretty closely matches the ACL2s
  version. This is helped by the fact that both languages are functional.
  The function checks contracts and delegates the actual work. The
  delegated function practically matches the ACL2s verison while
  utilizing Haskell to code golf a bit.

  Since Haskell is a stackically typed and compiled language, I have to
  add the recognizers manually. This ensures that the inputs and
  output conform to the dynamic checks. The type system also helps
  since you can't do something like give the function a number, it
  at least has to be a BoolFm.
  -}

  putStrLn "Examples for part 3:"
  print . constPropNorFm . stf $ "(nil !v p)"
  print . constPropNorFm . stf $ "(t !v p)"
  print . constPropNorFm . stf $ "(q !v p)"
  print . constPropNorFm . stf $ "((p !v t) !v p)"
  putStrLn ""

  {-
  In addition to the requirements, I implemented boolFm->norFm
  (mainly since I had already done it before the requirements
  were posted).

  I also implemented parsing of S-Expressions so
  that these examples are easier to show. I ended up using a
  library for the SExpr handling. Figuring out packages and stuff
  is very painful (but this was probably good practice).

  I was able to perform property based testing by using the QuickCheck
  library. The main difficult was creating the functions that generate
  the arbitrary data to be tested. It was helpful in finding bugs though.

  If anything, this challenge has made me realize even more how much
  of an effect idioms will have on code. Many things I did was pushed
  towards certain designs just because that is how the GHC library works
  or from the other libraries that I used to help with parsing and testing.
  -}

  let example :: String -> IO ()
      example s = do
        let f = stf s
        putStrLn "Given Formala: "
        print f
        putStrLn "BoolFm -> NorFm -> NorCPFm: "
        print . constPropNorFm . boolFmToNorFm $ f
        putStrLn ""

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
