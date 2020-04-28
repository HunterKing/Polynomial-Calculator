coefficient :: [String] -> [Integer]
coefficient [] = []
coefficient ((x:cs):xs)
    | (x == '-') = readNumber (findValue (x:cs)) : coefficient xs
    | (x == '+') = readNumber (findValue cs) : coefficient xs
    | (isChar x) = 1 : coefficient xs
    | otherwise = coefficient xs

readNumber :: String -> Integer
readNumber (s:ss)
    | ((s:ss) == "-") = -1 
    | otherwise = read (s:ss) :: Integer


findValue :: String -> String
findValue [] = ""
findValue (s:ss) = if (isNum s || s == '-') then [s] ++ findValue ss else findValue ""

isNum :: Char -> Bool
isNum c = '0' <= c && c <= '9'

isChar :: Char -> Bool
isChar c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')