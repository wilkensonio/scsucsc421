
module Main where 

a::Integer -> Integer -> Integer
a n m = n + m

exOr :: Bool -> Bool -> Bool
exOr a b = (a || b) && not (a && b)

-- fib :: Integer -> Integer
--     | n == 1  = 1
--     | n == 2  = 1
--     | n > 2   = fib (n-1) + fib (n-2)
    

main :: IO ()

main = do  
    let result = a 100 2   
    putStrLn (show result)

    putStrLn (show (exOr True True))
    

-- main = putStrLn "Hello, World!"


-- main = do
--     putStrLn "Hello, World!"






