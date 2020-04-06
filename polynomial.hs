main = do
	putStrLn "Enter a divisor polynomial"
	rawform <- getLine
	--We need to assign our input to a variable to parse, but we can use putStrLn to assume it is a string for input purposes.
	--let form = read rawform :: String
	putStrLn "Enter a dividend polynomial"
	rawform2 <- getLine
	--let form2 = read rawform2 :: String
	putStrLn(show rawform)
	putStrLn(show rawform2)
