
concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ concat xs

sum' :: [Integer] -> Integer
sum' [] = 0
sum' (x:xs) = x + sum' xs

length' :: [a] -> Integer
length' [] = 0
length' (x:xs) = 1 + length' xs

prodc :: [Integer] -> Integer
prodc [] = 1
prodc (x:xs) = x * prodc xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

index :: [a] -> Integer -> a
index (x:xs) 0 = x
index (x:xs) n = index xs (n-1)

pow2 :: Integer -> Integer
pow2 0 = 1
pow2 n = 2 * pow2 (n-1)

head' :: [a] -> a
head' (x:xs) = x

tail' :: [a] -> [a]
tail' (x:xs) = xs

last' :: [a] -> a
last' [a] = a
last' (x:xs) = last' xs

init' :: [a] -> [a]
init' [a] = []
init' (x:xs) = x : init' xs

splitAt' :: Integer -> [a] -> ([a], [a])
splitAt' n xs = (take n xs, drop n xs)

zip' :: [a] -> [b] -> [(a, b)]
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

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

boooks :: Database -> Person -> [Book]
books db per = [b | (p', b) <-  per == p]

borrowers :: Database -> Book -> [Person]
borrowers db b = [p | (p, b') <- db, b == b']

borrowed :: Database -> Book -> Bool
borrowed db b = length (borrowers db b) > 0



numBorrowed :: Database -> Person -> Integer
numBorrowed db p = length (books db p)



doubleAllEven :: [Integer] -> [Integer]
doubleAllEven xs = [2* x | x <- xs, x `mod` 2 == 0]

type Database = [(Person, Book)]

main :: IO()
main = do 
    putStrLn (show (index [1..10] 3))
    putStrLn (show (pow2 4))


