main = do
    putStrLn "Enter a divisor"
    rawform <- getLine
    
    putStrLn(show (splitNeg (splitPos rawform)))

splitPos :: String -> [String]
splitPos x = split '+' x

splitNeg :: [String] -> [String]
splitNeg [(x:xs)] = ["-"] ++ split '-' x : splitNeg [(x:xs)]

--Splits a string into a list of strings around a delimiter character.
split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s