module Contract.Recognizer where

type Recognizer t = t -> Bool

allp :: Recognizer a
allp _ = True
