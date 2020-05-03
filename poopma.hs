findValue :: String -> String
findValue [] = ""
findValue (s:ss) = if (isNum s) then [s] ++ findValue ss else "1" ++ findValue ""

isNum :: Char -> Bool
isNum c = '0' <= c && c <= '9'