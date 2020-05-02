combineEqs :: [(Integer,Integer)] -> [(Integer,Integer)]
comineEqs [] = []
combineEqs ((x1,x2):(y1,y2):rest) 
   | (x2 == y2) = ((x1 + y1), x2) : rest
   | otherwise = (x1, x2) : combineEqs ((y1,y2):rest)

hasDuplicate :: [(Integer,Integer)] -> [(Integer,Integer)]
hasDuplicate [] = []
hasDuplicate [x] = x : hasDuplicate []
hasDuplicate ((x1,x2):(y1,y2):rest) 
   | (x2 == y2) = hasDuplicate (combineEqs ((x1,x2):(y1,y2):rest))
   | otherwise = (x1,x2) : hasDuplicate ((y1,y2):rest) 

multiplyEqs :: [(Integer,Integer)] -> [(Integer,Integer)] -> [(Integer,Integer)]
multiplyEqs [] y = []
multiplyEqs [x] [] = x : multiplyEqs [] []
multiplyEqs ((x1,x2):rest1) ((y1,y2):rest2) 
   | (null ((x1,x2):rest1)) = multiplyEqs [] ((y1,y2):rest2) 
   | otherwise = map (x1 * y1,x2 + y2) : multiplyEqs rest1 ((y1,y2):rest2) 
