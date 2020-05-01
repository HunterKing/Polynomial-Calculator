import Data.List.Split
import Data.List

buildTuple :: [String] -> [Integer] -> [(String, Integer)]
buildTuple [] [] = []
buildTuple [s] [] = []
buildTuple [] [x] = []
buildTuple (s:ss) (x:xs)
    | (x >= 0) = [findCoef x (s:ss)] ++ buildTuple (s:ss) xs
    | otherwise = buildTuple (s:ss) []

findCoef :: Integer -> [String] -> (String, Integer)
findCoef n [] = (mempty,0)
findCoef n (t:ts) 
   | (isInfixOf ("^" ++ (show n)) t) = (pullCoef 0 t, n)
   | n == 1 = (pullCoef 0 t, n) 
   | n == 0 = (pullCoef 0 t, n)
   | otherwise = findCoef n ts

pullCoef :: Integer -> String -> String
pullCoef n "" = ""
pullCoef n (x:y:ss) 
    | (x == '+') = "+" ++ pullCoef 0 (y:ss)
    | (x == '-') = "-" ++ pullCoef 0 (y:ss)
    | (isNum x) = [x] ++ pullCoef 1 (y:ss) 
    | (y == '^' && n == 0) = "1" ++ pullCoef 0 ""
    | ((isChar x) && (n == 0)) = "1" ++ pullCoef 0 "" 
    | otherwise =  pullCoef 0 ""

isNum :: Char -> Bool
isNum c = '0' <= c && c <= '9'

isChar :: Char -> Bool
isChar c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')
