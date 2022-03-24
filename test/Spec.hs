import           Assignment
import           BoolFm
import           Compiler
import           Control.Monad
import           Operation.Binary
import           Operation.Unary
import           Test.QuickCheck

instance Arbitrary UnOp where
  arbitrary = oneof $ map return [Not]

instance Arbitrary BinOp where
  arbitrary = oneof $ map return [And, Or, Imp, Eq, Xor, Nand, Nor]

instance Arbitrary BoolFm where
  arbitrary = sized f
    where
      f 0 = return T
      f 1 = return Nil
      f 2 = do
        oneof $ map (return . V) ['a' .. 'z']
      f 3 = do
        op <- arbitrary
        n <- choose (0, 5)
        p <- f n
        return $ U op p
      f 4 = do
        op <- arbitrary
        n1 <- choose (0, 5)
        p <- f n1
        n2 <- choose (0, 5)
        q <- f n2
        return $ B op p q
      f n | n > 0 = return Nil

prop_varp :: Char -> Bool
prop_varp c = elem c ['a' .. 'z'] <= varp c

prop_assignmentp :: [Char] -> Bool
prop_assignmentp a = all (`elem` ['a' .. 'z']) a <= assignmentp a

prop_subtype_boolFmp :: BoolFm -> Bool
prop_subtype_boolFmp f
  | norCPFmp f = all ($ f) [norFmp, boolFm1p, boolFmp]
  | norNCFmp f = all ($ f) [norFmp, boolFm1p, boolFmp]
  | norFmp f = all ($ f) [boolFm1p, boolFmp]
  | boolFm1p f = all ($ f) [boolFmp]
  | boolFm1p f = True
  | otherwise = False

prop_boolFmToNorFm :: BoolFm -> Bool
prop_boolFmToNorFm f =
  if norFmp f
    then f == boolFmToNorFm f
    else f /= boolFmToNorFm f

prop_constPropNorFm :: BoolFm -> Bool
prop_constPropNorFm f
  | norFmp f = norFmp $ constPropNorFm f
  | otherwise = True

main :: IO ()
main = do
  putStrLn ""
  quickCheckWith stdArgs {maxSuccess = 10000} prop_varp
  quickCheckWith stdArgs {maxSuccess = 10000} prop_assignmentp
  quickCheckWith stdArgs {maxSuccess = 10000} prop_subtype_boolFmp
  quickCheckWith stdArgs {maxSuccess = 10000} prop_boolFmToNorFm
  quickCheckWith stdArgs {maxSuccess = 10000} prop_constPropNorFm
  putStrLn "Finished"
