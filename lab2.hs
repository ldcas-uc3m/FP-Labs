-- Lab 2: Sections, guards, and patterns
-- By Luis Daniel Casais Mezquida



-- EXERCISE 1
-- Define a square function using sections
square :: Int -> Int
square x = (* x) x


-- EXERCISE 2
-- Write a function older name1 age1 name2 age2 that receives two names and two ages and prints who is older. Example: if we introduce `older "Pepe" 23 Luisa" 18`, it must print `Pepe is older than Luisa`. If they are the same age it must print `Pepe and Luisa are the same age`.

older :: [Char] -> Int -> [Char] -> Int -> [Char]
older name1 age1 name2 age2
    | age1 > age2 = name1 ++ " is older than " ++ name2
    | age2 > age1 = name2 ++ " is older than " ++ name1
    | otherwise = name2 ++ " and " ++ name1 ++ " are the same age"


-- EXERCISE 3
-- Write a function that prints on the screen the price of a cinema ticket according to the age of the customer. 
--  (a) Normal ticket: 9 Euros;
--  (b) Children under 5: 60% discount;
--  (c) People over 60: 55% discount;
--  (d) Young people under 26: 20% discount.
-- Create a version with guards and another with patterns.

ticketPrice :: Int -> Float
ticketPrice age
    -- has to go in ascending order
    | age < 5 = 0.4 * price
    | age < 26 = 0.8 * price
    | age > 60 = 0.55 * price
    | age <= 0 = 0
    | otherwise = price

    where price = 9


ticketPrice' :: Int -> Float
ticketPrice' 60 = 0.55 * 9
ticketPrice' 26 = 9
ticketPrice' 5 = 0.8 * 9
ticketPrice' 4 = 0.4 * 9
ticketPrice' 1 = 0.4 * 9
ticketPrice' 0 = 0
ticketPrice' x = ticketPrice' (x - 1)  -- subtract 1 to age until it reaches one of the base cases (kinda like a floor function)


-- TODO: EXERCISE 4
-- Create a function receiving a number of seconds and converting it to its hours equivalent (for example 3680 seconds are 1 hour, 1 minute and 20 seconds and will be printed like 01:01:20). Notice the leading zeros. You can use auxiliary functions if needed.

secondsToHours seconds =
    0


-- EXERCISE 5
-- Create a function that receives the coordinates of a point in a plane, i.e. two integer values, x and y, not equal to zero. The function must return the quadrant where this point lies (1st quadrant if x>0 and y>0, 2nd if x<0 and y>0, etc.) For example for (1,1) it must return: "1st". If x or y are zero it must print "The values are not valid".

quadrant :: Int -> Int -> [Char]
quadrant x y
    | (x > 0) && (y > 0) = "1st"
    | (x > 0) && (y < 0) = "2nd"
    | (x < 0) && (y < 0) = "3rd"
    | (x < 0) && (y > 0) = "4th"
    | otherwise = "The values are not valid"


-- EXERCISE 6
-- A year is leap-year if it is a multiple of 4, except if it is a multiple of 100. In this last case it will be leap-year only if it is also a multiple of 400. For example the year 1900 was not a leap-year, but the year 2000 was. 
-- Create a function that calculates leap years. You can use auxiliary functions if needed. 
-- Example of outputs (notice the use of past or future tense):
-- - 1901 was not a leap year
-- - 2016 was a leap year
-- - 2400 will be a leap year
-- - 2401 will not be a leap year
-- - 2023 is not a leap year

isLeap :: Int -> [Char]
isLeap year
-- " not" if it's not a leap year, otherwise ""
    | year `mod` 400 == 0 = ""
    | (year `mod` 4 == 0) && (year `mod` 100 /= 0) = ""
    | otherwise = " not"


leapVerb :: Int -> [Char]
leapVerb year
-- calculates verb tense depending on year (plus adds leap or no leap)
    | year > currentYear = " will" ++ isLeap year ++ " be"
    | year < currentYear = " was" ++ isLeap year
    | year == currentYear = " is" ++ isLeap year

    where currentYear = 2023

leapYear :: Int -> [Char]
leapYear year = show year ++ leapVerb year ++ " a leap year"



-- EXERCISE 7
-- Create a function to simulate a calculator. It must receive two integer numbers and the operator (+ - * div) and show the result. If the operator is not valid it must raise an error "wrong operator" and finish.

calculator :: Int -> Int -> [Char] -> Int
calculator num1 num2 operator
    | operator == "+" = num1 + num2
    | operator == "-" = num1 - num2
    | operator == "*" = num1 * num2
    | operator == "div" = num1 `div` num2
    | otherwise = error "wrong operator"


main :: IO ()
main = do

    putStrLn "-- EXERCISE 1 --"
    putStrLn ""

    putStrLn ("square 2: " ++ show (square 2))

    putStrLn ""
    putStrLn "-- EXERCISE 2 --"
    putStrLn ""

    putStrLn ("older \"Pepe\" 22 \"Luisa\" 18: " ++ older "Pepe" 22 "Luisa" 18)
    putStrLn ("older \"Pepe\" 12 \"Luisa\" 18: " ++ older "Pepe" 12 "Luisa" 18)
    putStrLn ("older \"Pepe\" 18 \"Luisa\" 18: " ++ older "Pepe" 18 "Luisa" 18)

    putStrLn ""
    putStrLn "-- EXERCISE 3 --"
    putStrLn ""

    putStrLn ("ticketPrice 3: " ++ show (ticketPrice 3))
    putStrLn ("ticketPrice 22: " ++ show (ticketPrice 22))
    putStrLn ("ticketPrice 61: " ++ show (ticketPrice 61))
    putStrLn ("ticketPrice 29: " ++ show (ticketPrice 29))

    putStrLn ""

    putStrLn ("ticketPrice' 3: " ++ show (ticketPrice' 3))
    putStrLn ("ticketPrice' 22: " ++ show (ticketPrice' 22))
    putStrLn ("ticketPrice' 61: " ++ show (ticketPrice' 61))
    putStrLn ("ticketPrice' 29: " ++ show (ticketPrice' 29))

    putStrLn ""
    putStrLn "-- EXERCISE 4 --"
    putStrLn ""

    putStrLn ("quadrant 0 1: " ++ show (quadrant 0 1))
    putStrLn ("quadrant 1 0: " ++ show (quadrant 1 0))
    putStrLn ("quadrant 1 1: " ++ show (quadrant 1 1))
    putStrLn ("quadrant 1 (-1): " ++ show (quadrant 1 (-1)))
    putStrLn ("quadrant (-1) (-1): " ++ show (quadrant (-1) (-1)))
    putStrLn ("quadrant (-1) 1: " ++ show (quadrant (-1) 1))

    putStrLn ""
    putStrLn "-- EXERCISE 5 --"
    putStrLn ""

    putStrLn ("leapYear 1901: " ++ show (leapYear 1901))
    putStrLn ("leapYear 2016: " ++ show (leapYear 2016))
    putStrLn ("leapYear 2400: " ++ show (leapYear 2400))
    putStrLn ("leapYear 2401: " ++ show (leapYear 2401))
    putStrLn ("leapYear 2023: " ++ show (leapYear 2023))

    putStrLn ""
    putStrLn "-- EXERCISE 6 --"
    putStrLn ""

    putStrLn ("calculator 2 2 \"+\": " ++ show (calculator 2 2 "+"))
    putStrLn ("calculator 2 2 \"-\": " ++ show (calculator 2 2 "-"))
    putStrLn ("calculator 2 2 \"*\": " ++ show (calculator 2 2 "*"))
    putStrLn ("calculator 2 2 \"div\": " ++ show (calculator 2 2 "div"))
    putStrLn ("calculator 2 2 \"^\": " ++ show (calculator 2 2 "^"))
