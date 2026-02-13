import Data.Map qualified as Map
import Shapes

main = do
  print $ nudge (baseCircle 10) 2 3

data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

vmult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmult` m = Vector (i * m) (j * m) (k * m)

data Person = Person
  { firstNmae :: String,
    lastName :: String,
    age :: Int
  }
  deriving (Eq, Show, Read)

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
  Nothing -> Left $ "Locker " ++ show lockerNumber ++ " dose'nt exist!"
  Just (state, code) ->
    if state /= Taken
      then Right code
      else Left $ "Locker " ++ show lockerNumber ++ " is alrady taken!"
