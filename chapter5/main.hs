applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

main :: IO ()
main = do
  print (applyTwice (+3) 10)
  print (applyTwice (++ " HAHA") "HEY")
  print (applyTwice (3:) [1])
