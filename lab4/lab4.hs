-- Lab 4
-- By Luis Daniel Casais Mezquida



-- EXERCISE 1
-- Use list comprehension to create a function that returns the position of all the even elements in a list
evenPositions :: [Int] -> [Int]
evenPositions x = [fst pair | pair <- zip [0..] x, even (snd pair)]  -- map index to values, select indexes where values are even



-- EXERCISE 2
-- Create a function that returns the position of the first even element of a list or -1 if there are no even elements on the list.

safehead :: [Int] -> Int
-- returns -1 if the list is empty
safehead [] = -1
safehead x = head x

posFirstEven :: [Int] -> Int
posFirstEven xs = safehead (evenPositions xs)



-- EXERCISE 3
-- Use the previous function to create a function that splits a list at its first even element (it will be part of the second list)
splitEven :: [Int] -> ([Int], [Int])
splitEven x = splitAt (posFirstEven x) x



-- EXERCISE 4
-- Create a function that given a list of elements tells if they are consecutive either in ascending or descending order.

consecutive :: (Enum a, Eq a) => [a] -> Bool
consecutive [] = True
consecutive [x] = True
consecutive (x:y:xs) = (y == succ x || x == succ y) && consecutive (y:xs)


-- without recursion

pairs :: [a] -> [(a, a)]
-- generates pairs of consecutive elements of a list
pairs xs = zip xs (tail xs)

consecutive' :: (Enum a, Eq a) => [a] -> Bool
consecutive' xs = and [y == succ x || x == succ y | (x,y) <- pairs xs]



-- EXERCISE 5
-- A number is perfect if it is equal to the addition of all its divisors, except itself. Create a function that returns a list with all the perfect numbers until the given one (included). 
-- Note: auxiliary functions can be created.

divisors :: Int -> [Int]
-- returns divisors of an integer
divisors x = [n | n <- [1..x], x `rem` n == 0]

perfect :: Int -> Bool
-- checks if an integer is perfect
perfect x = x == sum (divisors x) - x

perfectTo :: Int -> [Int]
perfectTo x = [n | n <- [1..x `div` 2], perfect n]  -- max divisor will be x/2



-- EXERCISE 6
-- Create a function that returns the addition of all the multiples of 3 or 5 to the number n included.

multiples3_5 :: Int -> Int
multiples3_5 n = sum [x | x <- [3..n], x `rem` 3 == 0 || x `rem` 5 == 0]


-- EXERCISE 7
-- Use types to create:

-- 1. A type to represent a Date with day (Int), month (Int), year (Int)
type Date = (Int, Int, Int)

day :: Date -> Int
day (day, month, year) = day

month :: Date -> Int
month (day, month, year) = month

year :: Date -> Int
year (day, month, year) = year

-- 2. A function to return if the year of a Date is a leap year
isLeap :: Date -> Bool
isLeap date
    | y `mod` 400 == 0 = True
    | (y `mod` 4 == 0) && (y `mod` 100 /= 0) = True
    | otherwise = False
    where y = year date

-- 3. A function to check if a month number is correct
rightMonth :: Date -> Bool
rightMonth date
    | m > 0 && m <= 12 = True
    | otherwise = False
    where m = month date

-- 4. A function to check if a day number is correct for that month of that Date
rightDay :: Date -> Bool
rightDay date
    | d <= 0 = False
    | m == 2 && ((isLeap date && d <= 29) || d <= 28) = True  -- February
    | m `elem` long_months && d <= 31 = True
    | m `elem` short_months && d <= 30 = True
    | otherwise = False
    where m = month date
          d = day date
          y = year date
          long_months = [1,3,5,7,8,10,12]
          short_months = [4,6,9,11]

-- 5. A function that if the month of the day is incorrect, fixes them, the month will be 1 and the day will be 1 too.
fixDate :: Date -> Date
fixDate date
    | not (rightMonth date) || not (rightDay date) = (1, 1, y)
    | otherwise = date
    where y = year date

-- 6. A function that returns the name of the month of that date
monthName :: Date -> String
monthName date = months !! (m - 1)
    where m = month date
          months = ["January","February","March","April","May","June","July","August","September","October","November","December"]


-- 7. A function that returns "day of monthName of the leap/not leap year"

leapQualifier :: Date -> String
-- returns "" if leap year, "not" if not leap
leapQualifier date
    | isLeap date = ""
    | otherwise = "not "

showDate :: Date -> String
showDate date = show (day date) ++ " of " ++ monthName date ++ " of the " ++ leapQualifier date ++ "leap year"


-- 8. A function that returns the next Date of a given one
nextDay :: Date -> Maybe Date
nextDay date
    | not (rightMonth date) || not (rightDay date) = Nothing
    | m == 12 && d == 31 = Just (1,1,y+1)
    | m == 2 && ((isLeap date && d <= 29) || d == 28) = Just (1,m+1,y)  -- February
    | m `elem` long_months && d == 31 = Just (1,m+1,y)
    | m `elem` short_months && d == 30 = Just (1,m+1,y)
    | otherwise = Just (d+1,m,y)
    where m = month date
          d = day date
          y = year date
          long_months = [1,3,5,7,8,10,12]
          short_months = [4,6,9,11]

-- 9. A function that returns if date1 is previous to date2
isPrevious :: Date -> Date -> Bool
isPrevious date1 date2
    | y1 < y2 = True
    | y1 > y2 = False
    -- as we're using guards, we already know y1 == y2 in subsequent cases
    | m1 < m2 = True
    | m1 > m2 = False
    -- as we're using guards, we already know m1 == m2 in subsequent cases
    | d1 < d2 = True
    | d1 > d2 = False
    | otherwise = False  -- same day
    where m1 = month date1
          d1 = day date1
          y1 = year date1
          m2 = month date2
          d2 = day date2
          y2 = year date2


main :: IO ()
main = do

    putStrLn "-- EXERCISE 1 --"
    putStrLn ""

    putStrLn $ "evenPositions []: " ++ show (evenPositions [])
    putStrLn $ "evenPositions [1, 1, 1, 1]: " ++ show (evenPositions [1, 1, 1, 1])
    putStrLn $ "evenPositions [1, 1, 1, 2]: " ++ show (evenPositions [1, 1, 1, 2])
    putStrLn $ "evenPositions [2, 2, 2, 2]: " ++ show (evenPositions [2, 2, 2, 2])

    putStrLn ""
    putStrLn "-- EXERCISE 2 --"
    putStrLn ""

    putStrLn $ "posFirstEven []: " ++ show (posFirstEven [])
    putStrLn $ "posFirstEven [1, 1, 1, 1]: " ++ show (posFirstEven [1, 1, 1, 1])
    putStrLn $ "posFirstEven [1, 1, 1, 2]: " ++ show (posFirstEven [1, 1, 1, 2])
    putStrLn $ "posFirstEven [2, 2, 2, 2]: " ++ show (posFirstEven [2, 2, 2, 2])

    putStrLn ""
    putStrLn "-- EXERCISE 3 --"
    putStrLn ""

    putStrLn $ "splitEven []: " ++ show(splitEven [])
    putStrLn $ "splitEven [1, 1, 1, 1]: " ++ show(splitEven [1, 1, 1, 1])
    putStrLn $ "splitEven [1, 1, 1, 2]: " ++ show(splitEven [1, 1, 1, 2])
    putStrLn $ "splitEven [2, 2, 2, 2]: " ++ show(splitEven [2, 2, 2, 2])

    putStrLn ""
    putStrLn "-- EXERCISE 4 --"
    putStrLn ""

    putStrLn $ "perfectTo 10: " ++ show(perfectTo 10)
    putStrLn $ "perfectTo 1000: " ++ show(perfectTo 1000)
    putStrLn $ "perfectTo 10000: " ++ show(perfectTo 10000)

    putStrLn ""
    putStrLn "-- EXERCISE 5 --"
    putStrLn ""

    putStrLn $ "multiples3_5 2: " ++ show(multiples3_5 2)
    putStrLn $ "multiples3_5 3: " ++ show(multiples3_5 3)
    putStrLn $ "multiples3_5 5: " ++ show(multiples3_5 5)
    putStrLn $ "multiples3_5 15: " ++ show(multiples3_5 15)

    putStrLn ""
    putStrLn "-- EXERCISE 6 --"
    putStrLn ""

    putStrLn $ "consecutive [0..5]: " ++ show (consecutive [0..5])
    putStrLn $ "consecutive ['a'..'x']: " ++ show (consecutive ['a'..'x'])
    putStrLn $ "consecutive ['a', 'd'..'x']: " ++ show (consecutive ['a', 'd'..'x'])
    putStrLn $ "consecutive [2, 1, 0, -1]: " ++ show (consecutive [2, 1, 0, -1])

    putStrLn ""

    putStrLn $ "consecutive' [0..5]: " ++ show (consecutive' [0..5])
    putStrLn $ "consecutive' ['a'..'x']: " ++ show (consecutive' ['a'..'x'])
    putStrLn $ "consecutive' ['a', 'd'..'x']: " ++ show (consecutive' ['a', 'd'..'x'])
    putStrLn $ "consecutive' [2, 1, 0, -1]: " ++ show (consecutive' [2, 1, 0, -1])

    putStrLn ""
    putStrLn "-- EXERCISE 7 --"
    putStrLn ""

    putStrLn $ "isLeap (6,9,2020): " ++ show (isLeap (6,9,2020))
    putStrLn $ "isLeap (6,9,2021): " ++ show (isLeap (6,9,2021))
    putStrLn $ "rightMonth (4,2,1970): " ++ show (rightMonth (4,2,1970))
    putStrLn $ "rightMonth (4,20,1970): " ++ show (rightMonth (4,20,1970))
    putStrLn $ "rightDay (29,2,2000): " ++ show (rightDay (29,2,2000))
    putStrLn $ "rightDay (29,2,2001): " ++ show (rightDay (29,2,2001))
    putStrLn $ "rightDay (31,3,2001): " ++ show (rightDay (31,3,2001))
    putStrLn $ "fixDate (29,2,2001): " ++ show (fixDate (29,2,2001))
    putStrLn $ "fixDate (29,2,2000): " ++ show (fixDate (29,2,2000))
    putStrLn $ "showDate (29,2,2000): " ++ show (showDate (29,2,2000))
    putStrLn $ "showDate (22,2,2001): " ++ show (showDate (22,2,2001))
    putStrLn $ "nextDay (22,2,2001): " ++ show (nextDay (22,2,2001))
    putStrLn $ "nextDay (31,12,2001): " ++ show (nextDay (31,12,2001))
    putStrLn $ "nextDay (29,2,2000): " ++ show (nextDay (29,2,2000))
    putStrLn $ "nextDay (4,20,1970): " ++ show (nextDay (4,20,1970))
    putStrLn $ "isPrevious (29,2,2000) (1,3,2000): " ++ show (isPrevious (29,2,2000) (1,3,2000))
    putStrLn $ "isPrevious (29,2,2000) (1,1,2000): " ++ show (isPrevious (29,2,2000) (1,1,2000))
    putStrLn $ "isPrevious (29,2,2000) (1,1,2001): " ++ show (isPrevious (29,2,2000) (1,1,2001))
