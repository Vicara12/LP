printer:: String -> IO ()
printer (c:cs)
    | c == 'A' || c == 'a'  = putStrLn "Hello!"
    | otherwise = putStrLn "Bye!"

main = do
    name <- getLine
    printer name
