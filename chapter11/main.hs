import Data.Char
import Data.List

main = do
  line <- fmap (intersperse '-' . reverse . map toUpper) getLine
  putStrLn line

data CMayby a = CNothing | CJust Int a deriving (Show)

instance Functor CMayby where
  fmap f CNothing = CNothing
  fmap f (CJust counter x) = CJust (counter + 1) (f x)
