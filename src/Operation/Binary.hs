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
  readsPrec _ ('^' : s)       = [(And, s)]
  readsPrec _ ('v' : s)       = [(Or, s)]
  readsPrec _ ('=' : '>' : s) = [(Imp, s)]
  readsPrec _ ('=' : s)       = [(Eq, s)]
  readsPrec _ ('<' : '>' : s) = [(Xor, s)]
  readsPrec _ ('!' : '^' : s) = [(Nand, s)]
  readsPrec _ ('!' : 'v' : s) = [(Nor, s)]
  readsPrec _ _               = []

bop :: BinOp -> Bool -> Bool -> Bool
bop And  = (&&)
bop Or   = (||)
bop Imp  = (<=)
bop Eq   = (==)
bop Xor  = (/=)
bop Nand = \a b -> not $ a && b
bop Nor  = \a b -> not $ a || b
