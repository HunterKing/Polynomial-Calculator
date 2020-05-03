combineEqs :: [(Integer,Integer)] -> [(Integer,Integer)]
combineEqs [] = []
combineEqs [x] = x : combineEqs []
combineEqs ((x1,x2):(y1,y2):rest) 
   | (x2 == y2) = ((x1 + y1), x2) : rest
   | otherwise = (x1, x2) : combineEqs ((y1,y2):rest)

hasDuplicate :: [(Integer,Integer)] -> [(Integer,Integer)]
hasDuplicate [] = []
hasDuplicate [x] = x : hasDuplicate []
hasDuplicate ((x1,x2):(y1,y2):rest) 
   | (x2 == y2) = hasDuplicate (combineEqs ((x1,x2):(y1,y2):rest))
   | otherwise = (x1,x2) : hasDuplicate ((y1,y2):rest) 

-- multiplyEqs :: [(Integer,Integer)] -> [(Integer,Integer)] -> [(Integer,Integer)]
-- -- multiplyEqs [] y = []
-- multiplyEqs [x] y = x : []
-- multiplyEqs ((x1,x2):rest) y 
--    | (not (null (x1,x2))) = a ++ (multiplyEqs rest y)
--    | otherwise = multiplyEqs [] y
--       where a = (zip (map (x1*) (splitListC y)) (map (x2+) (splitListP y)))

multiplyEqs :: [(Integer,Integer)] -> [(Integer,Integer)] -> [(Integer,Integer)]
multiplyEqs [(x1,x2)] y = combineEqs (zip (map (x1*) (splitListC y)) (map (x2+) (splitListP y))) ++ []
multiplyEqs ((x1,x2):rest) y = combineEqs (zip (map (x1*) (splitListC y)) (map (x2+) (splitListP y))) ++ (multiplyEqs rest y)




splitListC :: [(Integer,Integer)] -> [Integer]
splitListC [] = []
splitListC [(x,y)] = x : splitListC []
splitListC ((x,y):rest) = x : splitListC rest


splitListP :: [(Integer,Integer)] -> [Integer]
splitListP [] = []
splitListP [(x,y)] = y : splitListP []
splitListP ((x,y):rest) = y : splitListP rest

