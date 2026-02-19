import Data.Char

main = do
  putStrLn "What's your first name?"
  firstName <- getLine
  putStrLn "What's your last name?"
  lastName <- getLine
  let bigFirstName = map toUpper firstName
      bigLasName = map toUpper lastName
  putStrLn $
    "hey "
      ++ bigFirstName
      ++ " "
      ++ bigLasName
      ++ ", how are you?"

  putStr "Hey, "
  putStr "I'm "
  putStrLn "Andy!s"
