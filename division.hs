--[(1,4), (2,3), (-2,1), (-1,0)]
--               [(1,1), (-1,0)]

divide :: [(Integer, Integer)] -> [(Integer, Integer)] -> [(Integer, Integer)]
--Cannot divide two empty lists.
divide [] [] = []
--If we try to divide by zero, fail.
divide [x] [] = [x]
--If we try to divide zero by something, return zero.
divide [] [y] = [(0,0)]
--Otherwise, proceed.
divide ((x1,x2):xs) ((y1,y2):ys)
    --If the degree of the term being divided is greater than that it is being divided by, multiply that term until it is equal.
    | (x2 > y2) = (multiply ys ys x2)
    | otherwise = []

multiply :: [(Integer, Integer)] -> [(Integer, Integer)] -> Integer -> [(Integer, Integer)]
--Cannot multiply two empty lists.
multiply [] [] _ = []
--If we try to multiply a list by an empty list, return itself.
multiply [x] [] _ = [x]
multiply [] [y] _ = [y]
--Otherwise, multiply until the degree passed as an Integer is met.
multiply ((x1,x2):xs) ((y1,y2):ys) n
  | (x2 > y2) =  [(0,0)]
  | otherwise = []
  let z1 = x1 * y1
  let z2 = x2 + y2
  --z1 = x1 * y1