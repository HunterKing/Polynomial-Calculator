--124x^4+3x^3-x+1

import Data.List.Split
import Data.List

main = do
    putStrLn "Enter a divisor"
    rawform <- getLine
    putStrLn(show (splitString rawform))
    putStrLn(show (degreeFinder (splitString rawform)))
    putStrLn(show (coefficient(splitString rawform)))
    

splitString :: String -> [String]
splitString x = removeEmpty (split (keepDelimsL $ oneOf "+-^") x)

degreeFinder :: [String] -> [Integer]
degreeFinder [] = []
degreeFinder ((x:cs):xs) = if (x == '^') then expListBuild [readNumber cs] else degreeFinder xs

coefficient :: [String] -> [Integer]
coefficient [] = []
coefficient ((x:cs):xs)
    | (x == '-') = readNumber (findValue (x:cs)) : coefficient xs
    | (x == '+') = readNumber (findValue cs) : coefficient xs
    | (isChar x) = 1 : coefficient xs
    | otherwise = coefficient xs

-- helper Functios
--Convert a string into an Integer value.

readNumber :: String -> Integer
readNumber (s:ss)
    | ((s:ss) == "-") = -1 
    | otherwise = read (s:ss) :: Integer


findValue :: String -> String
findValue [] = ""
findValue (s:ss) = if (isNum s || s == '-') then [s] ++ findValue ss else findValue ""

expListBuild :: [Integer] -> [Integer]
expListBuild [] = [0]
expListBuild [x] = if (x>0) then x : expListBuild [x - 1] else expListBuild []

isNum :: Char -> Bool
isNum c = '0' <= c && c <= '9'

isChar :: Char -> Bool
isChar c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot pred = filter $ not . pred

removeEmpty :: [String] -> [String]
removeEmpty x = filterNot (`elem` [""]) x