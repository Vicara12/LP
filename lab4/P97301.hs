-- FizzBuzz

fizzbuzzer :: Int -> Either Int String
fizzbuzzer i
    | (mod i 3 == 0) && (mod i 5 == 0) = Right "FizzBuzz"
    | mod i 3 == 0 = Right "Fizz"
    | mod i 5 == 0 = Right "Buzz"
    | otherwise = Left i

fizzBuzz :: [Either Int String]
fizzBuzz = map fizzbuzzer [0 ..]
