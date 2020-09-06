# Polynomial Calculator
This program is a simpled polynomial calculator in Haskell. We can Add, Subtract, Multiply, and Divide polynomials. This program was developed in collaboration between [Hunter King](https://github.com/HunterKing/) and [Joe Story](https://github.com/storyjss) as a final project for our Programming Languages course.

## Usage:
You can open the program using `ghci polynomial.hs`, and call each of the core functions multiplyPolynomials, dividePolynomials, subtractPolynomials, and addPolynomials with 2 tuple lists that represent a polynomial.

For example, if you wanted to add the two polynomials (3x^2+x)+(2x-4) you can call `addPolynomials [(3,2),(1,1)] [(2,1),(-4,0)]`, and the output would be `[(3,2),(3,1),(-4,0)]`.
