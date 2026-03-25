main :: IO ()
main = do
  let val = (+) <$> Just 3 <*> Just 5
  print val
