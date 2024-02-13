 
-- Write a definition of a function called add.
-- It should take two Integers (a and b) as inputs and an Integer as the output. The result should be
-- the addition of two input numbers (a+b)

add::Integer -> Integer -> Integer
add a b = a + b

-- Write a definition of a function called double. 
-- It should take one Integer as the input (n) and an 
-- Integer as the output. The function should double the value of the input value n, which is 2n.

double::Integer -> Integer
double n = n * 2

-- Write a definition of a function called area to calculate the area of a circle.
-- It should take one Float as the input (r) and a Float as the output

area::Float -> Float
area r = pi * r^2

-- Write a definition of a function called cube.
--  to calculate the cube of an input n. Both input and output types are Integer

cube::Integer -> Integer
cube n = n^3

-- Write a definition of a function to double the area of a circle.
-- The function only takes one Float input. You should reuse the function area you defined

doubleArea::Float -> Float
doubleArea r = area r * 2

-- Write a definition of a function called cylinder to calculate the volume of a cylinder.
-- The function takes two input, a Float (r) and a height (h). You should reuse the function area you defined

cylinder::Float -> Float -> Float
cylinder r h = area r * h

-- Write a function to convert Celsius to Fahrenheit.
-- It takes a Float input (c) and produces a Float output (f).
-- (Formula:(c×9/5)+32.)

celsiusToFahrenheit::Float -> Float
celsiusToFahrenheit c = (c * 9/5) + 32

-- Give another version of the definition of exclusive or which works informally like this:
-- exclusive or of x and y will be True if either x is True and y is False, or x is False and y is True.

xOr::Bool -> Bool -> Bool
xOr x y
    | x == True && y == False = True
    | x == False && y == True = True
    | otherwise = False


-- Give two different definitons of the nAnd function:

nAnd::Bool -> Bool -> Bool
nAnd x y
    | x == True && y == True = True 
    | otherwise = False


-- Write a function min to find the minimum number of two given numbers.
-- The function take two integers as input and return an integer as output.
 

_min::Integer -> Integer -> Integer
_min a b
    | a < b = a
    | otherwise = b 


areaTrian::Float -> Float -> Float -> Float
areaTrian a b c = sqrt(s*(s-a)*(s-b)*(s-c))
    where s = (a+b+c)/2



main :: IO()
main = do
    putStrLn $ show(add 5 6)

    putStrLn $ show(double 25)

    putStrLn $ show(area 5.0)

    putStrLn $ show(cube 5)

    putStrLn $ show(doubleArea 5.0)

    putStrLn $ show(cylinder 5.0 10.0)

    putStrLn $ show(celsiusToFahrenheit 5.0)

    putStrLn $ show(xOr False False)

    putStrLn $ show(nAnd False False)

    putStrLn $ show(_min 10 6)

    putStrLn $ show(areaTrian 3 4 5)