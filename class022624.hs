
evenNum :: [Integer] -> [Integer]
evenNum x = [n | n <- x, mod n 2 == 0]


isDigit :: Char -> Bool
isDigit x = x `elem` ['0'..'9']
 
 
-- add to end of list
addEnd :: [Integer] -> Integer -> [Integer]
addEnd x y = x ++ [y]
  
-- index
index :: [Integer] -> Integer -> Integer
index (x:xs) 0 = x
index (x:xs) n = index xs (n-1)


replicate' :: Integer -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x

drop' :: Integer -> [a] -> [a]
drop' 0 x = x
drop' n (x:xs) = drop' (n-1) xs

take' :: Integer -> [a] -> [a]
take' 0 x = []
take' n (x:xs) = x : take' (n-1) xs

slitAt' :: Integer -> [a] -> ([a], [a])
slitAt' 0 x = ([], x)
slitAt' n (x:xs) = (x : fst (slitAt' (n-1) xs), snd (slitAt' (n-1) xs))

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []

UNZIP' :: [(a, b)] -> ([a], [b])
UNZIP' [] = ([], [])
UNZIP' ((x, y):xs) = (x : fst (UNZIP' xs), y : snd (UNZIP' xs))

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]




main :: IO ()
main = do 
    -- putStrLn (show (length [1..10]))
    -- putStrLn (show (3: [1..10]) )
    -- putStrLn show ([1..10])
    putStrLn (show (index [1..10] 3))
    putStrLn (show (replicate' 5 3))