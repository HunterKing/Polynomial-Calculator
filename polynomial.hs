main = do
	putStrLn "Enter a divisor polynomial"
	rawform <- getLine
	let form = read rawform :: String
	putStrLn "Enter a dividend polynomial"
	rawform <- getLine
	let form2 = read rawform :: String
	putStrLn(show form)
	putStrLn(show form2)
