class PrintAllType t where
    printAll' :: [String] -> t

instance PrintAllType (IO a) where
    printAll' acc = do mapM_ putStrLn acc
                       return undefined

instance (Show a, PrintAllType r) => PrintAllType (a -> r) where
    printAll' acc = \x -> printAll' (acc ++ [" " ++ (show $ length acc)] ++ [show x])

printAll :: (PrintAllType t) => t
printAll = printAll' []

main :: IO ()
main = do printAll 5 "Mary" "had" "a" "little" "lamb" 4.2 -- note: the arguments can even be different types
          printAll 4 3 5
