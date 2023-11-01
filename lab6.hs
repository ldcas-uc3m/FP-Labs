-- Lab 6: Recursion and High Order Functions
-- By Luis Daniel Casais Mezquida



-- EXERCISE 1
-- Create a function that given two lists returns if the elements of the first list are divisible by the equivalent elements of the second list.

divisible :: [Int] -> [Int] -> [Bool]
divisible [] ys = []
divisible xs [] = [False]
divisible (x:xs) (y:ys) = (x `mod` y == 0) : divisible xs ys


-- EXERCISE 2
-- Create a function that given two lists returns the elements of the first list that are divisible by the equivalent elements of the second list.

selectDivisible :: [Int] -> [Int] -> [Int]
selectDivisible [] ys = []
selectDivisible xs [] = []
selectDivisible (x:xs) (y:ys)
    | x `mod` y == 0 = x : selectDivisible xs ys
    | otherwise = selectDivisible xs ys


-- EXERCISE 3
-- Create a function that returns the list resulting of applying the function f over the lists xs and ys.

apply :: (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]
apply f xs [] = []
apply f [] ys = []
apply f (x:xs) (y:ys) = f x y : apply f xs ys


-- EXERCISE 4
-- Create a function that returns if an element belongs to any of the sublists.

elem' :: (Eq t) => t -> [[t]] -> Bool
elem' x = foldr ((||) . elem x) False

-- elem' x [] = False
-- elem' x (y:ys) = elem x y || elem' x ys


-- EXERCISE 5
-- Create a function that returns the largest multiple of a number under a limit

largestMultiple :: Int -> Int -> Int
largestMultiple number limit = last (takeWhile (< limit) [number * n | n <- [1..]])



-- EXERCISE 6
-- Create a function that returns all the multiples of a number between a range

takeAllMultiples :: Int -> Int -> Int -> [Int]
takeAllMultiples number lower upper = filter (> lower) (takeWhile (< upper) [number * n | n <- [1..]])


-- EXERCISE 7
-- Create a function that returns a list with the common elements of two lists (without repetitions).

commonElements :: (Eq a) => [a] -> [a] -> [a]
commonElements [] ys = []
commonElements (x:xs) ys
    | x `elem` ys = x : commonElements (filter (/= x) xs) ys  -- filter out the repeated elements to avoid repetition
    | otherwise = commonElements xs ys


-- EXERCISE 8
-- Create a that returns a list with the common elements of a list and a list of lists (without repetitions).

commonElements' :: (Eq a) => [a] -> [[a]] -> [a]
commonElements' [] ys = []
commonElements' (x:xs) ys
    | elem' x ys = x : commonElements' (filter (/= x) xs) ys
    | otherwise = commonElements' xs ys


-- EXERCISE 9
-- Create a function that receives a list and returns the result of adding all the even numbers and substracting the odd ones

-- Adds number to the sum if even, substracts if odd
_evenOrOdd :: Int -> Int -> Int
_evenOrOdd acc x
    | even x = acc + x
    | odd x = acc - x

addEvenSubtractOdd :: [Int] -> Int
addEvenSubtractOdd [] = 0
addEvenSubtractOdd xs = foldl _evenOrOdd 0 xs

addEvenSubtractOdd' :: [Int] -> Int
addEvenSubtractOdd' xs = sum (filter even xs) - sum (filter odd xs)



main :: IO ()
main = do

    putStrLn "-- EXERCISE 1 --"
    putStrLn ""

    putStrLn $ "divisible [1, 2, 3] [1, 2, 3] = " ++ show (divisible [1, 2, 3] [1, 2, 3])
    putStrLn $ "divisible [1, 2, 3, 4] [2, 3, 4] = " ++ show (divisible [1, 2, 3, 4] [2, 3, 4])
    putStrLn $ "divisible [1, 2] [1, 2, 3, 4] = " ++ show (divisible [1, 2] [1, 2, 3, 4])


    putStrLn ""
    putStrLn "-- EXERCISE 2 --"
    putStrLn ""

    putStrLn $ "selectDivisible [1, 2, 3] [1, 2, 3] = " ++ show (selectDivisible [1, 2, 3] [1, 2, 3])
    putStrLn $ "selectDivisible [1, 2, 3, 4] [2, 3, 4] = " ++ show (selectDivisible [1, 2, 3, 4] [2, 3, 4])
    putStrLn $ "selectDivisible [1, 2] [1, 2, 3, 4] = " ++ show (selectDivisible [1, 2] [1, 2, 3, 4])


    putStrLn ""
    putStrLn "-- EXERCISE 3 --"
    putStrLn ""

    putStrLn $ "apply (+) [1, 2, 3] [4, 5, 6] = " ++ show (apply (+) [1, 2, 3] [4, 5, 6])
    putStrLn $ "apply mod [4, 5, 6] [1, 2, 3] = " ++ show (apply mod [4, 5, 6] [1, 2, 3])
    putStrLn $ "apply (+) [1, 2, 3, 4] [4, 5, 6] = " ++ show (apply (+) [1, 2, 3, 4] [4, 5, 6])
    putStrLn $ "apply mod [4, 5, 6] [1, 2, 3, 4] = " ++ show (apply mod [4, 5, 6] [1, 2, 3, 4])
    putStrLn $ "apply (*) [] [] = " ++ show (apply (*) [] [])


    putStrLn ""
    putStrLn "-- EXERCISE 4 --"
    putStrLn ""

    putStrLn $ "elem' 1 [[1,2], [3, 4]] = " ++ show (elem' 1 [[1,2], [3, 4]])
    putStrLn $ "elem' 2 [[]] = " ++ show (elem' 2 [[]])
    putStrLn $ "elem' 2 [[3], [3, 4], [5, 6]] = " ++ show (elem' 2 [[3], [3, 4], [5, 6]])


    putStrLn ""
    putStrLn "-- EXERCISE 5 --"
    putStrLn ""

    putStrLn $ "largestMultiple 2 10 = " ++ show (largestMultiple 2 10)
    putStrLn $ "largestMultiple 3 120 = " ++ show (largestMultiple 3 120)
    putStrLn $ "largestMultiple 32 31389 = " ++ show (largestMultiple 32 31389)


    putStrLn ""
    putStrLn "-- EXERCISE 6 --"
    putStrLn ""

    putStrLn $ "takeAllMultiples 32 200 3189 = " ++ show (takeAllMultiples 32 200 3189)
    putStrLn $ "takeAllMultiples 32 100 50 = " ++ show (takeAllMultiples 32 100 50)
    putStrLn $ "takeAllMultiples 32 200 3189 = " ++ show (takeAllMultiples 32 200 3189)
    putStrLn $ "takeAllMultiples 32 100 50 = " ++ show (takeAllMultiples 32 100 50)


    putStrLn ""
    putStrLn "-- EXERCISE 7 --"
    putStrLn ""

    putStrLn $ "commonElements [2, 1] [1, 2, 3, 4, 1, 2, 3] = " ++ show (commonElements [2, 1] [1, 2, 3, 4, 1, 2, 3])
    putStrLn $ "commonElements [1, 1, 2] [] = " ++ show (commonElements [1, 1, 2] [])
    putStrLn $ "commonElements [1, 1, 2, 5, 1] [1, 2, 3, 4, 1, 2] = " ++ show (commonElements [1, 1, 2, 5, 1] [1, 2, 3, 4, 1, 2])


    putStrLn ""
    putStrLn "-- EXERCISE 8 --"
    putStrLn ""

    putStrLn $ "commonElements' [1, 1, 2, 5, 3] [[1, 2, 3], [4, 1, 2]] = " ++ show (commonElements' [1, 1, 2, 5, 3] [[1, 2, 3], [4, 1, 2]])

    putStrLn ""
    putStrLn "-- EXERCISE 9 --"
    putStrLn ""

    putStrLn $ "addEvenSubtractOdd [1, 2, 3, 4, 5, 6] = " ++ show (addEvenSubtractOdd [1, 2, 3, 4, 5, 6])
    putStrLn $ "addEvenSubtractOdd' [1, 2, 3, 4, 5, 6] = " ++ show (addEvenSubtractOdd' [1, 2, 3, 4, 5, 6])