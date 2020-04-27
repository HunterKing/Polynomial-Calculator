import Data.List.Split
import Data.List

main = do
    putStrLn "Enter a divisor"
    rawform <- getLine
    putStrLn(show (removeEmpty (splitString rawform)))
    

splitString :: String -> [String]
splitString x = split (keepDelimsL $ oneOf "+-") x

removeEmpty :: [String] -> [String]
removeEmpty x = filterNot (`elem` [""]) x

tuplema :: [String] -> [(Integer, Integer)]
tuplema x

eqToTuple :: String -> (Integer, Integer)
eqToTuple x = 

-- helper Functios
--Convert a string into an Integer value.
readNumber :: String -> Integer
readNumber s = read s :: Integer

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot pred = filter $ not . pred