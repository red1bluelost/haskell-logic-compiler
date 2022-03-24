module Operation.Unary (UnOp (..), uop) where

data UnOp = Not deriving (Eq)

instance Show UnOp where
  show Not = "!"

instance Read UnOp where
  readsPrec _ s = [(f s, "")]
    where
      f "!" = Not
      f _   = error "failed to parse binary operand"

uop :: UnOp -> Bool -> Bool
uop Not = not
