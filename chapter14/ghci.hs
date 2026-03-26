import Control.Monad

main :: IO ()
main = do
  let val = (+) <$> Just 3 <*> Just 5
  print val

  print $ join (Just (Just 9))

  print $ join [[1, 2, 3], [4, 5, 6]]
