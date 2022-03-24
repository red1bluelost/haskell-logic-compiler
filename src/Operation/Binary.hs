module Operation.Binary (BinOp (..), bop) where

data BinOp = And | Or | Imp | Eq | Xor | Nand | Nor deriving (Eq)

instance Show BinOp where
  show And  = "^"
  show Or   = "v"
  show Imp  = "=>"
  show Eq   = "="
  show Xor  = "<>"
  show Nand = "!^"
  show Nor  = "!v"

bop :: BinOp -> Bool -> Bool -> Bool
bop And  = (&&)
bop Or   = (||)
bop Imp  = (<=)
bop Eq   = (==)
bop Xor  = (/=)
bop Nand = \a b -> not $ a && b
bop Nor  = \a b -> not $ a || b
