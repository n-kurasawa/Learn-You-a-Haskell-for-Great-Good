import Control.Monad

main :: IO ()
main = do
  let val = (+) <$> Just 3 <*> Just 5
  print val

  print $ join (Just (Just 9))

  print $ join [[1, 2, 3], [4, 5, 6]]

  let g = (\x -> return (x + 1)) <=< (\x -> return (x * 100))
  print $ Just 4 >>= g

  let f = foldr (.) id [(+ 8), (* 100), (+ 1)]
  print $ f 1
