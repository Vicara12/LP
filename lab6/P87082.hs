computeBMI :: [String] -> Float
computeBMI [a,b] = m/(h*h)
    where
        m = read(a)::Float
        h = read(b)::Float

classifyBMI :: Float -> String
classifyBMI bmi
    | bmi < 18  = "underweight"
    | bmi < 25  = "normal weight"
    | bmi < 30  = "overweight"
    | bmi < 40  = "obese"
    | otherwise = "severely obese"

processUser :: String -> IO ()
processUser s = putStrLn (n++": "++(classifyBMI (computeBMI d)))
    where
        w = words s
        n = head w
        d = tail w

main = do
    line <- getLine
    if line /= "*" then do
        processUser line
        main
    else
        return ()
