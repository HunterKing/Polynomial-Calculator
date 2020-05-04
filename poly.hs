
-- main = do 
--     putStrLn "Enter a divisor in form (Coefficient"
--     x <- getLine
--     putStrLn "Enter a dividend"
--     y <- getLine
--     putStrLn (show (dividePolynomials x )) 



----------------- main functions --------------------------------
--Calls to multiplyEqs for every term in the list, and then sorts and combines terms before return.
multiplyPolynomials :: [(Integer,Integer)] -> [(Integer,Integer)] -> [(Integer,Integer)]
multiplyPolynomials x y = hasDuplicate (quicksort (multiplyEqs x y))

--Calls to divideEqs for each term, then sorts and combines terms and removes eliminated terms before return.
dividePolynomials :: [(Integer,Integer)] -> [(Integer,Integer)] -> [(Integer,Integer)]
dividePolynomials [] y = []
dividePolynomials x y = if ((findDegree y) < (findDegree x)) then answer ++ (dividePolynomials (cleanFront (subtractPolynomials x (cleanFront (multiplyPolynomials y answer)))) y) else answer
   where answer = cleanFront (hasDuplicate (quicksort (divideEqs [head x] [head y])))

--Calls to divideEqs for each term, then when the division is complete condenses and returns the remainder.
getRemainder :: [(Integer,Integer)] -> [(Integer,Integer)] -> [(Integer,Integer)]
getRemainder [] y = []
getRemainder x y = if ((findDegree y) < (findDegree x)) then (getRemainder (cleanFront (subtractPolynomials x (cleanFront (multiplyPolynomials y answer)))) y) else cleanFront (subtractPolynomials x (multiplyPolynomials answer y)) 
   where answer = cleanFront (hasDuplicate (quicksort (divideEqs [head x] [head y])))

--Inverts one list and adds it to the other in order to subtract items.
subtractPolynomials :: [(Integer,Integer)] -> [(Integer,Integer)] -> [(Integer,Integer)]
subtractPolynomials x y = hasDuplicate (quicksort (x++(inverseEq y)))

--Adds two lists together to combine items.
addPolynomials :: [(Integer,Integer)] -> [(Integer,Integer)] -> [(Integer,Integer)]
addPolynomials x y = hasDuplicate (quicksort (x++y))

------------------- helper functions ---------------------------
--If two terms have like degrees, combines the coefficients to merge.
combineEqs :: [(Integer,Integer)] -> [(Integer,Integer)]
combineEqs [] = []
combineEqs ((x1,x2):(y1,y2):rest) 
   | (x2 == y2) = ((x1 + y1), x2) : rest
   | otherwise = (x1, x2) : combineEqs ((y1,y2):rest)

--Checks if a list still has items within that are not merged together and calls to combinedEqs 
hasDuplicate :: [(Integer,Integer)] -> [(Integer,Integer)]
hasDuplicate [] = []
hasDuplicate [x] = [x] 
hasDuplicate ((x1,x2):(y1,y2):rest) 
   | (x2 == y2) = hasDuplicate (quicksort (combineEqs ((x1,x2):(y1,y2):rest)))
   | otherwise =  (x1,x2) : hasDuplicate ((y1,y2):rest) 

--Applies a single term to the rest of the list in order to multiply.
multiplyEqs :: [(Integer,Integer)] -> [(Integer,Integer)] -> [(Integer,Integer)]
multiplyEqs [(x1,x2)] y = zip (map (x1*) (splitListC y)) (map (x2+) (splitListP y)) ++ []
multiplyEqs ((x1,x2):rest) y =  zip (map (x1*) (splitListC y)) (map (x2+) (splitListP y)) ++ (multiplyEqs rest y)

--Applies a single term to the rest of the list in order to divide.
divideEqs :: [(Integer,Integer)] -> [(Integer,Integer)] -> [(Integer,Integer)]
divideEqs [(x1,x2)] y = zip (map (x1`div`) (splitListC y)) (map (x2-) (splitListP y)) ++ []
divideEqs ((x1,x2):rest) y =  zip (map (x1 `div`) (splitListC y)) (map (x2-) (splitListP y)) ++ (multiplyEqs rest y)

--Rips the coefficients from each tuple and returns them as a list.
splitListC :: [(Integer,Integer)] -> [Integer]
splitListC [] = []
splitListC [(x,y)] = x : splitListC []
splitListC ((x,y):rest) = x : splitListC rest

--Rips the degrees from each tuple and returns them as a list.
splitListP :: [(Integer,Integer)] -> [Integer]
splitListP [] = []
splitListP [(x,y)] = y : splitListP []
splitListP ((x,y):rest) = y : splitListP rest

--Applies a quicksort to the tuple list sorted by degree so that like terms are placed close together to combine.
quicksort :: [(Integer, Integer)] -> [(Integer, Integer)]  
quicksort [] = []  
quicksort ((x1,x2):xs) =   
    let smallerSorted = quicksort [ y | y <- xs, snd y >= x2]  
        biggerSorted = quicksort [y | y <- xs, snd y < x2]  
    in  smallerSorted ++ [(x1,x2)] ++ biggerSorted
 
--Inverts all the signs in one equation.
inverseEq :: [(Integer, Integer)] -> [(Integer, Integer)]
inverseEq [] = []
inverseEq ((x1,x2):rest) = ((x1 * (-1)),x2) : inverseEq rest
 
--Returns the degree in a tuple
findDegree :: [(Integer,Integer)] -> Integer
findDegree ((x1,x2):rest) = x2

--Removes terms in the front of a polynomial with a coefficient of zero.
cleanFront :: [(Integer,Integer)] -> [(Integer,Integer)]
cleanFront ((x1,x2):rest) = if (x1 == 0) then rest else ((x1,x2):rest)


