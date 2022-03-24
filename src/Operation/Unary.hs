module Operation.Unary (UnOp (..), uop) where

data UnOp = Not deriving (Eq)

instance Show UnOp where
  show Not = "!"

instance Read UnOp where
  readsPrec _ ('!' : s) = [(Not, s)]
  readsPrec _ _         = []

uop :: UnOp -> Bool -> Bool
uop Not = not
