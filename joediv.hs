--combineEqs :: [(Integer,Integer)] -> [(Integer,Integer)]
--combineEqs [] = []
--combineEqs [x] = x : combineEqs []
--combineEqs ((x1,x2):(y1,y2):rest) 
--  | (x2 == y2) = rest : [((x1 + y1), x2)]
--   | otherwise = combineEqs ((x1,x2):rest)

combineEqs :: [(Integer,Integer)] -> [(Integer,Integer)]
combineEqs [x] = [x]
combineEqs ((x1,x2):(y1,y2):rest) 
   | (x2 == y2) = ((x1 + y1), x2) : rest
   | otherwise = (x1, x2) : combineEqs ((y1,y2):rest)

hasDuplicate :: [(Integer,Integer)] -> [(Integer,Integer)]
hasDuplicate [] = []
hasDuplicate [x] = [x] 
hasDuplicate ((x1,x2):(y1,y2):rest) 
   | (x2 == y2) = hasDuplicate (combineEqs ((x1,x2):(y1,y2):rest))
   | otherwise = (x1,x2) : hasDuplicate ((x1,x2):rest) 

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

quicksort' :: (Ord a) => [a] -> [a]  
quicksort' [] = []  
quicksort' (x:xs) =   
    let smallerSorted = quicksort' [a | a <- xs, a >= x]  
        biggerSorted = quicksort' [a | a <- xs, a < x]  
    in  smallerSorted ++ [x] ++ biggerSorted  

quicksort :: [(Integer, Integer)] -> [(Integer, Integer)]  
quicksort [] = []  
quicksort ((x1,x2):xs) =   
    let smallerSorted = quicksort [ y | y <- xs, snd y >= x2]  
        biggerSorted = quicksort [y | y <- xs, snd y < x2]  
    in  smallerSorted ++ [(x1,x2)] ++ biggerSorted
  




