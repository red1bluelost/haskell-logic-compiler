module Contract.Output (checkOutput) where

import           Contract.Recognizer (Recognizer)

errorMessage :: String
errorMessage = "output contract has been violated"

checkOutput :: (Show o) => o -> [Recognizer o] -> o
checkOutput o recs =
  if not . all ($ o) $ recs
    then error $ errorMessage ++ ": " ++ show o
    else o
