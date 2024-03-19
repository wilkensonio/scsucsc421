type Database = [(Barcode, Name, Price)]

makebill :: Database -> [Barcode] -> (Name, Price)
makebill db barcodes = (name, price)
    where
        name = [n | (b, n, p) <- db, b `elem` barcodes]
        price = sum [p | (b, n, p) <- db, b `elem` barcodes]



integertofloattostring :: Integer -> Float
integertofloattostring n = fromInteger n /100

putStrLn (show (integertofloattostring 1000))
