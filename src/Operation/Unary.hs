module Operation.Unary (UnOp (..), uop) where

data UnOp = Not deriving (Eq)

instance Show UnOp where
  show Not = "!"

uop :: UnOp -> Bool -> Bool
uop Not = not
