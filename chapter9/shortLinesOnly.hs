import Control.Monad
import Data.Char

main = do
  contents <- getContents
  putStrLn (shortLineOnly contents)

shortLineOnly :: String -> String
shortLineOnly = unlines . filter (\line -> length line < 10) . lines
