-- Lab 1: Introduction to Haskell
-- By Luis Daniel Casais Mezquida



-- EXERCISE 1
-- Function that receives the number of coins of 1 cent, 2 cents, 5 cents, 10 cents, 20 cents and 50 cents and returns the equivalent in euros.
addCents :: Fractional a => a -> a -> a -> a -> a -> a -> a
addCents c1 c2 c5 c10 c20 c50 =
    c1 * 0.01 +
    c2 * 0.02 +
    c5 * 0.05 +
    c10 * 0.10 +
    c20 * 0.20 +
    c50 * 0.50


-- EXERCISE 2
-- Function that returns the maximum of three numbers.
maxOf3 :: Ord a => a -> a -> a -> a
maxOf3 a b c
    | (a >= b) && (a >= c) = a
    | (b >= a) && (b >= c) = b
    | (c >= a) && (c >= a) = c

maxOf3' :: Ord a => a -> a -> a -> a
maxOf3' a b c
    | a >= _maxOf2 b c = a
    | b >= _maxOf2 a c = b
    | c >= _maxOf2 a c = c

_maxOf2 :: Ord a => a -> a -> a
_maxOf2 a b
    | a >= b = a
    | otherwise = b


-- EXERCISE 3
-- Function that given three numbers returns the one in the middle of the three. For example middle 2 3 1 == 2
middle :: Ord a => a -> a -> a -> a
middle a b c
    | ((a >= b) && (a < c)) || ((a <= b) && (a > c)) = a
    | ((b >= a) && (b < c)) || ((b <= a) && (b > c)) = b
    | ((c >= a) && (c < b)) || ((c <= a) && (c > b)) = c


-- EXERCISE 4
-- Function that returns true if the values of all its parameters are equal. Check it works with any basic type. Check also the type of the function.
allEqual :: Eq a => a -> a -> a -> a -> Bool
allEqual a b c d
    | a /= b = False
    | a /= c = False
    | a /= d = False
    | otherwise = True


-- EXERCISE 5
-- Function that returns True if the values of all its parameters are different.
allDifferent :: Eq a => a -> a -> a -> Bool
allDifferent a b c
    | a == b = False
    | a == c = False
    | otherwise = True


-- EXERCISE 6
-- Function that returns True if the values of all its parameters are different. Consider reusing the previous one.
allDifferent' :: Ord a => a -> a -> a -> a -> Bool
allDifferent' a b c d =
    allDifferent b c d && (a > b)


-- EXERCISE 7
-- Function that receives two 1-digit numbers as parameters and returns the highest number that can be created combining both numbers. For example highestNumber 2 6 == 62
highestNumber :: (Ord a, Show a) => a -> a -> Int
highestNumber a b
    | a >= b = read (show a ++ show b) :: Int
    | b > a = read (show b ++ show a) :: Int



main :: IO ()
main = do
    putStrLn ("addCents 0 0 0 0 0 5: " ++ show (addCents 0 0 0 0 0 5))
    putStrLn ("maxOf3 3 4 4: " ++ show (maxOf3 3 4 4))
    putStrLn ("maxOf3' 3 4 4: " ++ show (maxOf3' 3 4 4))
    putStrLn ("middle 2 3 1: " ++ show (middle 2 3 1))
    putStrLn ("allEqual 0 0 0 0: " ++ show (allEqual 0 0 0 0))
    putStrLn ("allDifferent 0 1 3: " ++ show (allDifferent 0 1 3))
    putStrLn ("allDifferent' 3 0 1 0: " ++ show (allDifferent' 3 0 1 0))
    putStrLn ("highestNumber 2 1: " ++ show (highestNumber 2 1))