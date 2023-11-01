-- Lab 5: Types
-- By Luis Daniel Casais Mezquida

import Data.List (genericLength)


-- EXERCISE 1
-- Create:
-- 1. A new type to represent a student (name, surname) and his marks (list of Float)
data Student = Student {name :: String, surname :: String, marks :: [Float]} deriving Show

-- 2. Functions to return the name and the surname of the student

getName :: Student -> String
getName (Student name _ _) = name

-- 3. Functions to return the maximum, the minimum and the mean mark
maxi :: Student -> Float
maxi (Student _ _ marks) = maximum marks

mini :: Student -> Float
mini (Student _ _ marks) = minimum marks

mean :: Student -> Float
mean (Student _ _ marks) = sum marks / genericLength marks

-- 4. A function to count how many subjects the student passed
hasPassed :: Float -> Int
hasPassed subject
    | subject >= 5 = 1
    | otherwise = 0

count :: Student -> Int
count (Student _ _ marks) = foldl (\sum x -> sum + hasPassed x) 0 marks


-- EXERCISE 2
-- Make the Date type from previous week exercises to derive from `Ord` and redefine the ordering operators (< > >= <=) so they work properly.

data Date = Date {day :: Int, month :: Int, year :: Int} deriving (Show, Eq)
instance Ord Date where
    compare :: Date -> Date -> Ordering
    compare (Date dayA monthA yearA) (Date dayB monthB yearB) = compare [yearA, monthA, dayA] [yearB, monthB, dayB]  -- compare compares lists in order


    -- compare :: Date -> Date -> Ordering
    -- compare (Date dayA monthA yearA) (Date dayB monthB yearB)
    --     | yearA < yearB = LT
    --     | yearA > yearB = GT
    --     -- yearA == yearB in subsequent cases
    --     | monthA < monthB = LT
    --     | monthA > monthB = GT
    --     -- monthA == monthB in subsequent cases
    --     | dayA < dayB = LT
    --     | dayA > dayB = GT
    --     | otherwise = EQ  -- same day


-- EXERCISE 3
-- Make the Date type to derive from Enum as well, implementing `succ` and `pred` functions.

isLeap :: Date -> Bool
isLeap (Date _ _ y)
    | y `mod` 400 == 0 = True
    | (y `mod` 4 == 0) && (y `mod` 100 /= 0) = True
    | otherwise = False

isLeap' :: Int -> Bool
isLeap' y
    | y `mod` 400 == 0 = True
    | (y `mod` 4 == 0) && (y `mod` 100 /= 0) = True
    | otherwise = False


daysInMonth :: Int -> Int -> Int
daysInMonth month year
    | month `elem` [4, 6, 9, 11] = 30
    -- February
    | isLeap' year && month == 2 = 29
    | month == 2 = 28  -- already not leap
    | otherwise = 31

instance Enum Date where
    succ :: Date -> Date
    succ (Date d m y)
        | d < daysInMonth m y = Date (d + 1) m y
        | m < 12 = Date d (m + 1) y
        | otherwise = Date d m (y + 1)

    pred :: Date -> Date
    pred (Date d m y)
        | d > 1 = Date (d - 1) m y
        | m > 1 = Date (daysInMonth m y) (m - 1) y
        | otherwise = Date 31 12 (y - 1)

-- EXERCISE 4
-- Create a Product type with two constructors. The first one, named Product, will receive the name, the price and the units in stock of that product. The second one, FreshProduct, will receive also a Date (see last week exercises) with the expiration date.
data Product = Product {prod_name :: String, price :: Float, stock :: Int}
             | FreshProduct {prod_name :: String, price :: Float, stock :: Int, date :: Date}
    deriving Show

-- EXERCISE 5
-- Create a `sell Product` function that sells a unit of a product if its stock is bigger than 0, it returns the product with the new stock

sell :: Product -> Product
sell prod@(Product n p stock)
    | stock > 0 = Product n p (stock - 1)
    | otherwise = prod
sell prod@(FreshProduct n p stock d)
    | stock > 0 = FreshProduct n p (stock - 1) d
    | otherwise = prod


-- EXERCISE 6
-- Create a function `expired Date Product` that returns if the product has expired
expired :: Date -> Product -> Bool
expired Date {} Product {} = False
expired date@Date {} (FreshProduct _ _ _ expiration) = date > expiration


-- EXERCISE 7
-- Redefine the == operator as follows:
-- 1. Two Products are equal if their names and prices are equal
instance Eq Product where
    (==) :: Product -> Product -> Bool
    (==) (Product nA pA _) (Product nB pB _) = nA == nB && pA == pB

-- 2. Two FreshProducts are equal if in addition their expiration dates are equal
    (==) (FreshProduct nA pA _ eA) (FreshProduct nB pB _ eB) = nA == nB && pA == pB && eA == eB




main :: IO ()
main = do

    putStrLn "-- EXERCISE 1 --"
    putStrLn ""

    let student = Student "pepe" "perez" [1, 2, 3, 4, 5, 6, 7]
    putStrLn "let student = Student \"pepe\" \"perez\" [1, 2, 3, 4, 5, 6, 7]"

    putStrLn ""

    putStrLn $ "maxi student = " ++ show (maxi student)
    putStrLn $ "mini student = " ++ show (mini student)
    putStrLn $ "mean student = " ++ show (mean student)
    putStrLn $ "count student = " ++ show (count student)

    putStrLn ""
    putStrLn "-- EXERCISE 2 --"
    putStrLn ""

    putStrLn $ "Date 31 1 2022 > Date 30 1 2022 = " ++ show (Date 31 1 2022 > Date 30 1 2022)
    putStrLn $ "Date 30 1 2022 > Date 30 1 2022 = " ++ show (Date 30 1 2022 > Date 30 1 2022)
    putStrLn $ "Date 31 1 2022 >= Date 30 1 2022 = " ++ show (Date 31 1 2022 >= Date 30 1 2022)
    putStrLn $ "Date 30 1 2022 <= Date 30 1 2022 = " ++ show (Date 30 1 2022 <= Date 30 1 2022)

    putStrLn ""
    putStrLn "-- EXERCISE 3 --"
    putStrLn ""

    putStrLn $ "succ (Date 31 12 2022) = " ++ show (succ (Date 31 12 2022))
    putStrLn $ "succ (Date 31 1 2022) = " ++ show (succ (Date 31 1 2022))
    putStrLn $ "pred (Date 31 12 2022) = " ++ show (pred (Date 31 12 2022))
    putStrLn $ "pred (Date 31 1 2022) = " ++ show (pred (Date 31 1 2022))
    putStrLn $ "pred (Date 1 1 2022) = " ++ show (pred (Date 1 1 2022))
    putStrLn $ "pred (Date 1 12 2022) = " ++ show (pred (Date 1 12 2022))
    putStrLn $ "pred (Date 1 3 2022) = " ++ show (pred (Date 1 3 2022))
    putStrLn $ "pred (Date 1 3 2022) = " ++ show (pred (Date 1 3 2022))
    putStrLn $ "pred (Date 1 3 2020) = " ++ show (pred (Date 1 3 2020))

    putStrLn ""
    putStrLn "-- EXERCISE 4 --"
    putStrLn ""

    putStrLn $ "Product \"shoes\" 12.99 45 = " ++ show (Product "shoes" 12.99 45)
    putStrLn $ "Product \"oranges\" 1.18 33 (Date 1 1 2023) = " ++ show (FreshProduct "oranges" 1.18 33 (Date 1 1 2023))


    putStrLn ""
    putStrLn "-- EXERCISE 5 --"
    putStrLn ""

    putStrLn $ "sell (Product \"shoes\" 12.99 45) = " ++ show (sell (Product "shoes" 12.99 45))
    putStrLn $ "sell (Product \"oranges\" 1.18 33 (Date 1 1 2023)) = " ++ show (sell (FreshProduct "oranges" 1.18 33 (Date 1 1 2023)))

    putStrLn ""
    putStrLn "-- EXERCISE 6 --"
    putStrLn ""

    putStrLn $ "expired (Date 31 1 2023) (Product \"shoes\" 12.99 45) = " ++ show (expired (Date 31 1 2023) (Product "shoes" 12.99 45))
    putStrLn $ "expired (Date 31 1 2023) (FreshProduct \"oranges\" 1.18 33 (Date 1 1 2023)) = " ++ show (expired (Date 31 1 2023) (FreshProduct "oranges" 1.18 33 (Date 1 1 2023)))


    putStrLn ""
    putStrLn "-- EXERCISE 7 --"
    putStrLn ""

    putStrLn $ "Product \"shoes\" 12.99 45 == Product \"shoes\" 12.99 55 = " ++ show (Product "shoes" 12.99 45 == Product "shoes" 12.99 55)
    putStrLn $ "Product \"shoes\" 12.99 45 == Product \"shoes\" 1.99 55 = " ++ show (Product "shoes" 12.99 45 == Product "shoes" 1.99 55)
    putStrLn $ "FreshProduct \"oranges\" 1.18 33 (Date 1 1 2023) == FreshProduct \"oranges\" 1.18 12 (Date 1 1 2023) = " ++ show (FreshProduct "oranges" 1.18 33 (Date 1 1 2023) == FreshProduct "oranges" 1.18 12 (Date 1 1 2023))
    putStrLn $ "FreshProduct \"oranges\" 1.18 33 (Date 1 1 2023) == FreshProduct \"oranges\" 1.18 12 (Date 12 1 2023) = " ++ show (FreshProduct "oranges" 1.18 33 (Date 1 1 2023) == FreshProduct "oranges" 1.18 12 (Date 12 1 2023))
    putStrLn $ "FreshProduct \"oranges\" 1.18 33 (Date 1 1 2023) /= FreshProduct \"oranges\" 1.18 12 (Date 12 1 2023) = " ++ show (FreshProduct "oranges" 1.18 33 (Date 1 1 2023) /= FreshProduct "oranges" 1.18 12 (Date 12 1 2023))