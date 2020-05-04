
-- main = do 
--     putStrLn "Enter a divisor in form (Coefficient"
--     x <- getLine
--     putStrLn "Enter a dividend"
--     y <- getLine
--     putStrLn (show (dividePolynomials x )) 



----------------- main functions --------------------------------
multiplyPolynomials :: [(Integer,Integer)] -> [(Integer,Integer)] -> [(Integer,Integer)]
multiplyPolynomials x y = hasDuplicate (quicksort (multiplyEqs x y))

dividePolynomials :: [(Integer,Integer)] -> [(Integer,Integer)] -> [(Integer,Integer)]
dividePolynomials [] y = []
dividePolynomials x y = if ((findDegree y) < (findDegree x)) then answer ++ (dividePolynomials (cleanFront (subtractEqs x (cleanFront (multiplyPolynomials y answer)))) y) else answer
   where answer = cleanFront (hasDuplicate (quicksort (divideEqs [head x] [head y])))

subtractEqs :: [(Integer,Integer)] -> [(Integer,Integer)] -> [(Integer,Integer)]
subtractEqs x y = hasDuplicate (quicksort (x++(inverseEq y)))

addEqs :: [(Integer,Integer)] -> [(Integer,Integer)] -> [(Integer,Integer)]
addEqs x y = hasDuplicate (quicksort (x++y))

------------------- helper functions ---------------------------

getRemainder :: [(Integer,Integer)] -> [(Integer,Integer)] -> [(Integer,Integer)]
getRemainder [] y = []
getRemainder x y = if ((findDegree y) < (findDegree x)) then (getRemainder (cleanFront (subtractEqs x (cleanFront (multiplyPolynomials y answer)))) y) else cleanFront (subtractEqs x (multiplyPolynomials answer y)) 
   where answer = cleanFront (hasDuplicate (quicksort (divideEqs [head x] [head y])))

combineEqs :: [(Integer,Integer)] -> [(Integer,Integer)]
combineEqs [] = []
combineEqs ((x1,x2):(y1,y2):rest) 
   | (x2 == y2) = ((x1 + y1), x2) : rest
   | otherwise = (x1, x2) : combineEqs ((y1,y2):rest)

hasDuplicate :: [(Integer,Integer)] -> [(Integer,Integer)]
hasDuplicate [] = []
hasDuplicate [x] = [x] 
hasDuplicate ((x1,x2):(y1,y2):rest) 
   | (x2 == y2) = hasDuplicate (quicksort (combineEqs ((x1,x2):(y1,y2):rest)))
   | otherwise =  (x1,x2) : hasDuplicate ((y1,y2):rest) 

multiplyEqs :: [(Integer,Integer)] -> [(Integer,Integer)] -> [(Integer,Integer)]
multiplyEqs [(x1,x2)] y = zip (map (x1*) (splitListC y)) (map (x2+) (splitListP y)) ++ []
multiplyEqs ((x1,x2):rest) y =  zip (map (x1*) (splitListC y)) (map (x2+) (splitListP y)) ++ (multiplyEqs rest y)


inverseEq :: [(Integer, Integer)] -> [(Integer, Integer)]
inverseEq [] = []
inverseEq ((x1,x2):rest) = ((x1 * (-1)),x2) : inverseEq rest

divideEqs :: [(Integer,Integer)] -> [(Integer,Integer)] -> [(Integer,Integer)]
divideEqs [(x1,x2)] y = zip (map (x1`div`) (splitListC y)) (map (x2-) (splitListP y)) ++ []
divideEqs ((x1,x2):rest) y =  zip (map (x1 `div`) (splitListC y)) (map (x2-) (splitListP y)) ++ (multiplyEqs rest y)

splitListC :: [(Integer,Integer)] -> [Integer]
splitListC [] = []
splitListC [(x,y)] = x : splitListC []
splitListC ((x,y):rest) = x : splitListC rest

splitListP :: [(Integer,Integer)] -> [Integer]
splitListP [] = []
splitListP [(x,y)] = y : splitListP []
splitListP ((x,y):rest) = y : splitListP rest

quicksort :: [(Integer, Integer)] -> [(Integer, Integer)]  
quicksort [] = []  
quicksort ((x1,x2):xs) =   
    let smallerSorted = quicksort [ y | y <- xs, snd y >= x2]  
        biggerSorted = quicksort [y | y <- xs, snd y < x2]  
    in  smallerSorted ++ [(x1,x2)] ++ biggerSorted
  
findDegree :: [(Integer,Integer)] -> Integer
findDegree ((x1,x2):rest) = x2

cleanFront :: [(Integer,Integer)] -> [(Integer,Integer)]
cleanFront ((x1,x2):rest) = if (x1 == 0) then rest else ((x1,x2):rest)


