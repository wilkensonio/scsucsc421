
module Main where 

a::Integer -> Integer -> Integer
a n m = n + m

main :: IO ()

main = do  
    let result = a 100 2   
    putStrLn (show result)





