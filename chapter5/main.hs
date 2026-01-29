applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
-- flip' f = g
--   where g x y = f y x
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [b]
filter' _ [] = []
filter' p (x:xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs

main :: IO ()
main = do
  print (applyTwice (+3) 10)
  print (applyTwice (++ " HAHA") "HEY")
  print (applyTwice (3:) [1])
  let result1 = zipWith' (+) [4,2,5,6] [2,6,2,3]
      result2 = zipWith' max [6,3,2,1] [7,3,1,5]
      in do
        print result1
        print result2