

-- concat :: [[a]] -> [a]
-- concat [] = []
-- concat (x:xs) = [x] ++ concat xs

-- sum' :: [Integer] -> Integer
-- sum' [] = 0
-- sum' (x:xs) = x + sum' xs

-- length' :: [a] -> Integer
-- length' [] = 0
-- length' (x:xs) = 1 + length' xs

-- prodc :: [Integer] -> Integer
-- prodc [] = 1
-- prodc (x:xs) = x * prodc xs

-- reverse' :: [a] -> [a]
-- reverse' [] = []
-- reverse' (x:xs) = reverse' xs ++ [x]

-- index :: [a] -> Integer -> a
-- index (x:xs) 0 = x
-- index (x:xs) n = index xs (n-1)

-- pow2 :: Integer -> Integer
-- pow2 0 = 1
-- pow2 n = 2 * pow2 (n-1)

-- head' :: [a] -> a
-- head' (x:xs) = x

-- tail' :: [a] -> [a]
-- tail' (x:xs) = xs

-- last' :: [a] -> a
-- last' [a] = a
-- last' (x:xs) = last' xs

-- init' :: [a] -> [a]
-- init' [a] = []
-- init' (x:xs) = x : init' xs

-- splitAt' :: Integer -> [a] -> ([a], [a])
-- splitAt' n xs = (take n xs, drop n xs)

-- zip' :: [a] -> [b] -> [(a, b)]
-- zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

-- unzip' :: [(a, b)] -> ([a], [b])
-- unzip'  xs (left, right)
--     where 
--         left = [x | (x, y) <- xs]
--         right = [y | (x, y) <- xs]



-- return :: Person -> Book -> [(Person, Book)]

-- books :: Database -> Person -> [Book]
-- books [] _ = []
-- books ((p', b):db) p
--     | p' == p = b : books db p
--     | otherwise = books db p

-- boooks :: Database -> Person -> [Book]
-- books db per = [b | (p', b) <-  per == p]

-- borrowers :: Database -> Book -> [Person]
-- borrowers db b = [p | (p, b') <- db, b == b']

-- borrowed :: Database -> Book -> Bool
-- borrowed db b = length (borrowers db b) > 0



-- numBorrowed :: Database -> Person -> Integer
-- numBorrowed db p = length (books db p)



-- doubleAllEven :: [Integer] -> [Integer]
-- doubleAllEven xs = [2* x | x <- xs, x `mod` 2 == 0]

-- type Database = [(Person, Book)]

highestCommonFactor :: Integer -> Integer -> Integer
highestCommonFactor a 0 = a 
highestCommonFactor 0 b = b
highestCommonFactor a b = highestCommonFactor (b `mod` a) a
     

-- Give a definition of the function below
-- 	\[\text{orderTriple } :: (\text{Integer, Integer, Integer})  -> (\text{Integer, Integer, Integer}) \]
-- 	 which puts the elements of a triple of three integers into ascending order.

orderTriple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
orderTriple (x, y, z) 
    | x <= y && y <= z = (x, y, z)
    | x <= z && z <= y = (x, z, y)
    | y <= x && x <= z = (y, x, z)
    | y <= z && z <= x = (y, z, x)
    | z <= x && x <= y = (z, x, y)
    | z <= y && y <= x = (z, y, x)

tripleAll :: [Integer] -> [Integer]
tripleAll [] = []
-- tripleAll (x:xs) =[x*3] ++ tripleAll xs
tripleAll xs = [x*3 | x <- xs]


toUpper :: Char -> Char
toUpper c 
    | c >= 'a' && c <= 'z' = toEnum(fromEnum c - fromEnum 'a' + fromEnum 'A')  
    | otherwise = c


capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : capitalize xs

-- helper function
array :: Integer -> [Integer]
array n
    | n == 1 = [n]
    | otherwise =  array (n-1) ++ [n]

divisor :: Integer -> [Integer]
divisor n
    | n <= 0 = []
    | otherwise = [x | x <- (array n), n `mod` x == 0]

isPrime :: Integer -> Bool
isPrime n 
    | n <= 1 = False
    | otherwise = null [x | x <- (array (n-1)), n `mod` x == 0, x /= 1]


-- data Shape = Circle Float    -- Circle with radius
--            | Rectangle Float Float   -- Rectangle with length and width
--            | Square Float            -- Square with side length
--            deriving Show
-- perimeter :: Shape -> Float
-- perimeter (Circle radius) = 2 * pi * radius
-- perimeter (Rectangle length width) = 2 * (length + width)
-- perimeter (Square side) = 4 * side


squareRoot :: Integer -> Integer
squareRoot n = root n 0
    where 
        root n x
            | x * x > n = x - 1
            | otherwise = root n (x + 1)

multiplication :: Integer -> Integer -> Integer
multiplication _ 0 = 0
multiplication a b = a + multiplication a (b - 1)

division :: Integer -> Integer -> Integer
division 0 _ = 0 
division _ 0 = -10^99
division a b
    | a < b = 0
    | otherwise = 1 + division (a - b) b

print100 :: Integer -> [Integer]
print100 n = [x | x <- [1..n], x /= 1 , x `mod` 2 == 1]



upper :: Char -> Char
upper c 
    | c >= 'a' && c <= 'z' = toEnum(fromEnum c - fromEnum 'a' + fromEnum 'A')
    | otherwise = c 

middle :: Integer -> Integer -> Integer -> Integer
middle x y z 
    | x == z && z <= y = z
    | x <= y && x <= z = y
    | y <= x && x <= z = x
    | otherwise = z

data Move = Rock | Paper | Scissors
    deriving (Show,Eq)

beat :: Move -> Move
beat Rock = Paper
beat Paper = Scissors

len :: [a] -> Integer
len [] = 0
len (x:xs) = 1 + len xs

zip' :: [a] -> [b] -> [(a,b)]
zip' [] [] = []
zip' (x:xs) (y: ys) = (x,y) : zip' xs ys

unzip' :: [(a,b)] -> ([a], [b]) 
unzip' n = (left, right)
    where
        left = [x | (x, y) <- n]
        right = [y | (x, y) <- n]


type Shape = Double


integertofloattostring :: Integer -> Float
integertofloattostring n = fromInteger n /100


main :: IO()
main = do 
    -- putStrLn (show (index [1..10] 3))
    -- putStrLn (show (pow2 4))
    -- putStrLn (show (highestCommonFactor 18 24))
    -- putStrLn (show (orderTriple (2, 1, 3)))
    -- putStrLn (show (tripleAll [2,3,4]))
    -- 
    -- putStrLn (show (capitalize "forma fdfdsfldfjn  WILKENSON"))
    -- putStrLn (show (divisor (-12)))
    -- putStrLn (show (array 5))
    -- putStrLn (show (isPrime 12))
    -- putStrLn (show (chec 44))
    -- putStrLn (show (round (16**(1/2))))
    -- putStrLn (show (perimeter (Circle 4.0)))
    -- putStrLn (show (squareRoot 15))/
    -- putStrLn (show (multiplication 5 5))
    -- putStrLn (show (negate (16)))
 
    -- putStrLn (show(division 12 0))
    -- putStrLn(show (print100 100))
    -- putStrLn(show (middle 1 3 1))
    -- putStrLn(show (beat Rock))

    -- putStrLn ( show ( zip' ['q', 'f'] ['R','f']))
    -- putStrLn ( show (unzip' [(1,2)]))

    -- putStrLn ( show ([True, False]))
    putStrLn (show (integertofloattostring 1000))






