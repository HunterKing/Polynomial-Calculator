--124x^4+3x^3-x+1

import Data.List.Split
import Data.List

main = do
    putStrLn "Enter a divisor"
    rawform <- getLine
    putStrLn(show (splitString 1 rawform))
    putStrLn(show (degreeFinder (splitString 1 rawform)))
    putStrLn(show (zeroListBuild (length (degreeFinder (splitString 1 rawform)))))
    

splitString :: Integer -> String -> [String]
splitString 0 x = removeEmpty (split (keepDelimsL $ oneOf "+-^") x)
splitString 1 x = removeEmpty (split (keepDelimsL $ oneOf "+-") x)
degreeFinder :: [String] -> [Integer]
degreeFinder [] = []
degreeFinder ((x:cs):xs) = if (x == '^') then expListBuild [readNumber cs] else degreeFinder xs

-- makeCoefList :: Integer -> [String] -> [Integer]
-- makeCoefList 0 [x] = readNumber x               -- if degree is 0, polynomial is a constant
-- makeCeofList n lst =
--    let (x,y) = findCoefAndRemove n lst
--    in x : makeCoefList (n-1) y

findCoef :: Integer -> [String] -> (String, Integer)
findCoef n (t:ts) 
    | (isInfixOf "^n" t) = (findValue t, n)

-- findCoefAndRemove :: Integer -> [String] -> (Integer, [String])
-- findCoefAndRemove n (t : ts) =   "if t = ___^n then (___,ts) 
                                    -- else let (x,y) = findCoefAndRemove n ts
                                    --  in (x, t : y) "



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
