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

instance Read BinOp where
  readsPrec _ s = [(f s, "")]
    where
      f "^"  = And
      f "v"  = Or
      f "=>" = Imp
      f "="  = Eq
      f "<>" = Xor
      f "!^" = Nand
      f "!v" = Nor
      f _    = error "failed to parse binary operand"

bop :: BinOp -> Bool -> Bool -> Bool
bop And  = (&&)
bop Or   = (||)
bop Imp  = (<=)
bop Eq   = (==)
bop Xor  = (/=)
bop Nand = \a b -> not $ a && b
bop Nor  = \a b -> not $ a || b
