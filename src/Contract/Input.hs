module Contract.Input (checkInput1, checkInput2) where

import           Contract.Recognizer (Recognizer)

errorMessage :: String
errorMessage = "input contract has been violated"

checkInput1 :: (Show i1) => (i1 -> r) -> i1 -> [Recognizer i1] -> r
checkInput1 f i1 recs1 =
  if not . all ($ i1) $ recs1
    then error $ errorMessage ++ ": " ++ show i1
    else f i1

checkInput2 :: (Show i1) => (Show i2) => (i1 -> i2 -> r) -> i1 -> [Recognizer i1] -> i2 -> [Recognizer i2] -> r
checkInput2 f i1 recs1 i2 recs2 =
  if not . all ($ i2) $ recs2
    then error $ errorMessage ++ ": " ++ show i2
    else checkInput1 f i1 recs1 i2
