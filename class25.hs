removewhitespace:: String -> String
removewhitespace [] = []
removewhitespace (x:xs)
    | x == ' ' = removewhitespace xs
    | otherwise = x : removewhitespace xs

removeextrawhitespace:: String -> String
removeextrawhitespace [] = []
removeextrawhitespace (x:xs)
    | x == ' ' && head xs == ' ' = removeextrawhitespace xs
    | otherwise = x : removeextrawhitespace xs

wordtostring:: String -> [String]
wordtostring [] = []
wordtostring (x:xs)
    | x == ' ' = wordtostring xs
    | otherwise = [x] : wordtostring xs

dropword:: String -> String
dropword [] = []
dropword (x:xs)
    | x == ' ' = xs
    | otherwise = dropword xs