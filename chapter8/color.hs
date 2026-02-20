import Control.Monad

main = do
  colors <- forM [1, 2, 3, 4] $ \a -> do
    putStrLn $
      "Which color do you assosiate with the number "
        ++ show a
        ++ "?"
    getLine
  putStrLn "The colors that you assosiate with 1, 2, 3 and 4 are: "
  mapM putStrLn colors
