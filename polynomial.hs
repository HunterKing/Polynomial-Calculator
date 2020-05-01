--124x^4+3x^3-x+1

import Data.List.Split
import Data.List

main = do
    putStrLn "Enter a divisor"
    rawform <- getLine
    putStrLn(show (splitString rawform))
    putStrLn(show (degreeFinder (splitString rawform)))
    putStrLn(show (zeroListBuild (length (degreeFinder (splitString rawform)))))
    putStrLn(show (coef (zeroListBuild (length (degreeFinder (splitString rawform)))) (splitString rawform)))
    -- putStrLn(show (coefficient(splitString rawform)))
    

splitString :: String -> [String]
splitString x = removeEmpty (split (keepDelimsL $ oneOf "+-^") x)

degreeFinder :: [String] -> [Integer]
degreeFinder [] = []
degreeFinder ((x:cs):xs) = if (x == '^') then expListBuild [readNumber cs] else degreeFinder xs


coef :: [Integer] -> [String] -> [Integer]
coef [] [] = []
coef [] [s] = []
coef [x] [] = [x]
coef (x:xs) ((t:ts):ss)  
   | ((expo (t:ts)) > 0) = [0] --coef (replaceNth ((expo (t:ts)) - 1) (readNumber ts) (x:xs)) ss 
   | otherwise = coef (x:xs) [] 

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

zeroListBuild :: Int -> [Integer]
zeroListBuild 0 = []
zeroListBuild x = if (x>0) then 0 : zeroListBuild (x-1) else zeroListBuild 0  

isNum :: Char -> Bool
isNum c = '0' <= c && c <= '9'

isChar :: Char -> Bool
isChar c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot pred = filter $ not . pred

removeEmpty :: [String] -> [String]
removeEmpty x = filterNot (`elem` [""]) x

expo :: String -> Integer
expo [] = 0
expo (s:ss) = if (s == '^') then readNumber ss else expo ss

replaceNth :: Integer -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n-1) newVal xs
