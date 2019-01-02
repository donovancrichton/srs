%default total

data SumType = Tag1 | Tag2

buggy : IO (SumType)
buggy = do
  pure Tag1

main : IO ()
main = do
 Tag1 <- buggy | Tag2 => putStrLn "How did I get here?"
 putStrLn "Tag1"
