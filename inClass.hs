-- Author: wilkenson Hilarion
-- Date: 2024-12-02



areaTrian::Float -> Float -> Float -> Float
areaTrian a b c
    | triangle = sqrt(s*(s-a)*(s-b)*(s-c))
    | otherwise = 0
    where 
        triangle = a + b > c && a + c > b && b + c > a
        s = (a+b+c)/2
 

-- odd , even :: Int -> Bool

-- odd n
--     | n <= 0 = False
--     | otherwise = even (n-1)


--  rock paper scissors

data Move = Rock | Paper | Scissors
    deriving (Show, Eq)

beat :: Move -> Move
beat Rock = Paper
beat Paper = Scissors
beat Scissors = Rock

lose :: Move -> Move
lose Rock = Scissors
lose Paper = Rock
lose Scissors = Paper


-- factoraial

factorial :: Integer -> Integer
factorial n
    | n == 0 || n == 1 = 1
    | otherwise = n * factorial (n-1)
     

-- fibonacci  

fibonacci :: Integer -> Integer
fibonacci n
    | n == 1 = 1
    | n == 2 = 1
    | n > 1 = fibonacci (n-1) + fibonacci (n-2)

     



-- power of 2

powerOf2 :: Integer -> Integer
powerOf2 n
    | n == 0 = 1
    | n > 0 = 2 * powerOf2 (n-1)


tupple :: (Int, Int) -> (Int, Int) -> (Int, Int)
tupple (a, b) (c, d) = (a+c, b+d)   


main :: IO()    
main = do
    putStrLn $ show(areaTrian 5.0 6.0 7.0)
    putStrLn $ show(fibonacci 8)
    putStrLn $ show(factorial 5)

    let a = [1, 2, 3, 4, 5]
    putStrLn $ show([x*2 | x <- a])



-- convert to upper case

    toUpper :: Char -> Char
    toUpper c
        | c >= 'A' && c <= 'Z' = toEnum (fromEnum c - 32)
        | otherwise = c
     
    toUpperCase :: String -> String
    toUpperCase s = [toUpper c | c <- s]

    putStrLn $ toUpperCase "hello world"