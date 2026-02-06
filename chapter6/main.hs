import Data.List
import Data.Char
import qualified Data.Map as Map

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)

encode :: Int -> String -> String
encode offset msg = map (\c -> chr $ ord c + offset) msg

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

firstTo40 :: Maybe Int
firstTo40 = find (\x -> digitSum x == 40) [1..]

phoneBook :: Map.Map String String
phoneBook = Map.fromList $
    [("betty", "555-2938")   
    ,("bonnie", "452-2928")
    ,("patsy", "493-2928")   
    ,("lucille", "205-2928")   
    ,("wendy", "939-8282")
    ,("penny", "853-2492")
    ]

findkey :: (Eq k) => k -> [(k,v)] -> Maybe v
-- findkey key [] = Nothing
-- findkey key ((k,v):xs)
--     | key == k = Just v
--     | otherwise = findkey key xs

findkey key xs = foldr
                (\(k,v) acc -> if key == k then Just v else acc)
                Nothing xs
