import Control.Monad (foldM, liftM)
import Data.List

solveRPN :: String -> Maybe Double
solveRPN st = do
  [resutl] <- foldM foldingFunction [] (words st)
  return resutl

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x : y : ys) "*" = return ((y * x) : ys)
foldingFunction (x : y : ys) "+" = return ((y + x) : ys)
foldingFunction (x : y : ys) "-" = return ((y - x) : ys)
foldingFunction xs numberString = liftM (: xs) (readMaybe numberString)

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of
  [(x, "")] -> Just x
  _ -> Nothing
