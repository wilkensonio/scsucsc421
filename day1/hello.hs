
module Main where 

a::Integer -> Integer -> Integer
a n m = n + m

exOr :: Bool -> Bool -> Bool
exOr a b = (a || b) && not (a && b)

main :: IO ()

main = do  
    let result = a 100 2   
    putStrLn (show result)

    putStrLn (show (exOr True True))






