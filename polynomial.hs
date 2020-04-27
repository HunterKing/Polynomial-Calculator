import Data.List

main = do
    putStrLn "Enter a divisor polynomial"
    rawform <- getLine
    --We need to assign our input to a variable to parse, but we can use putStrLn to assume it is a string for input purposes.
    --let form = read rawform :: String
    putStrLn "Enter a dividend polynomial"
    rawform2 <- getLine
    --let form2 = read rawform2 :: String
    putStrLn(show (splitPos rawform))
    putStrLn(show (tupleify(splitPos rawform)))

--Checks if a character is a number.
isNum :: Char -> Bool
isNum c = '0' <= c && c <= '9'

--Takes a list of strings and return a list of coeff, exponent pairs.
tupleify :: [String] -> [(Integer, Integer)]
tupleify (x:xs) = [(readNumber(findValue x), 0)]

-- Take an input string and return it split as a list of coeff, variable, and exponent.
splitPos :: String -> [String]
splitPos (x:xs) = (split '+' x)

splitNeg :: String -> [String]
splitNeg (x:xs) = [combine "-" split "-" x] 

--Convert a string into an Integer value.
readNumber :: String -> Integer
readNumber s = read s :: Integer

--Will search a string for the numbers in the coeffiecient or the degree.
findValue :: String -> String
findValue [] = ""
findValue (s:ss) = if (isNum s) then [s] ++ findValue ss else "1" ++ findValue ""

findExp :: String -> String
findExp [] = ""
-- findExp (x:xs) = if (s == )

--Splits a string into a list of strings around a delimiter character.
split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s
-- split d (s:ss) = s : split d ss

