main = do
    putStrLn "Enter a divisor polynomial"
    rawform <- getLine
    --We need to assign our input to a variable to parse, but we can use putStrLn to assume it is a string for input purposes.
    --let form = read rawform :: String
    putStrLn "Enter a dividend polynomial"
    rawform2 <- getLine
    --let form2 = read rawform2 :: String
    -- putStrLn(show rawform)
    -- putStrLn(show rawform2)
    putStrLn(show (tupleify rawform))

isNum :: Char -> Bool
isNum c = '0' <= c && c <= '9'

tupleify :: String -> [(Integer, Integer)]
tupleify x = [(readNumber(takeStringWhile x), 0)]

readNumber :: String -> Integer
readNumber s = read s :: Integer

takeStringWhile :: String -> String
takeStringWhile [] = ""
takeStringWhile (s:ss) = if (isNum s) then [s] ++ takeStringWhile ss else takeStringWhile ""

--gaming
stringSplitter :: String -> (String)
stringSplitter [] = ""
stringSplitter (s:ss) 
    | s == '+' = append stringSplitter ss
    | s == '-' = '-' : stringSplitter ss
    | isNum s = 
    | otherwise = stringSplitter ""

gamingSplitter :: String -> (String)
gamingSplitter [] = ""


--takeStringWhile n:ns = if isNum n then append s (readNumber n) takeWhile ns
