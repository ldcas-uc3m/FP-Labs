-- Lab 7: Lists and High Order Functions
-- By Luis Daniel Casais Mezquida



-- EXERCISE 1
-- Create a count function using a fold

countr :: Int -> [Int] -> Int
countr elem = foldr (\ x count -> if x == elem then count + 1 else count) 0

countl :: Int -> [Int] -> Int
countl elem = foldl (\ count x -> if x == elem then count + 1 else count) 0


-- EXERCISE 2
-- Use a fold to create a function that returns a list with all the intermediate maximums of a list of integer numbers from left to right

allMax :: [Int] -> [Int]
allMax [] = []
-- save in accumulator the list of maximums. traverse the list, for each new maximum element, add it to the list of maximums
allMax xs = foldl (\ acc x -> if maximum acc < x then acc ++ [x] else acc) [head xs] xs


-- EXERCISE 3
-- Create a function that given a list of integer numbers returns the size of the biggest sequence of even numbers on it.

maxEvenStreak :: [Int] -> Int
-- Save in the accumulator a list of all streaks of even numbers. Traverse the list; if it's even, add one to the current streak (head of list), if it's odd, add a new (current) streak starting at 0, then just get the maximum of the streaks.
maxEvenStreak xs = maximum (
    foldl
        (\ (curr_streak:streaks) x ->
            if even x
                then (curr_streak + 1) : streaks
            else 0 : curr_streak : streaks
        )
        [0] xs
    )


-- EXERCISE 4
-- Create your own versions of the `all` and `any` higher order functions

myAll :: Foldable t => (a -> Bool) -> t a -> Bool
myAll op = foldl (\ acc x -> acc && op x) True


myAny :: Foldable t => (a -> Bool) -> t a -> Bool
myAny op = foldl (\ acc x -> acc || op x) True



-- EXERCISE 5
-- Create a new Set type composed by a list of non-repeated elements. Create the functions:
newtype Set a = Set [a] deriving (Show, Foldable)

-- that given a list creates a Set where the duplicated elments are removed
_removeDuplicates :: Eq a => [a] -> [a]
_removeDuplicates [] = []
_removeDuplicates (x:xs) = x : _removeDuplicates (filter (/= x) xs)

createSet :: Eq a => [a] -> Set a
createSet = Set . _removeDuplicates

-- that returns True if xs is a subset of ys
subset :: Eq a => Set a -> Set a -> Bool
subset xs ys = foldl (\ acc x -> acc && elem x ys) True xs

-- that returns True if xs and ys are equal (same elements despite of their order)
instance Eq a => Eq (Set a) where
    (==) :: Eq a => Set a -> Set a -> Bool
    xs == ys = subset xs ys && subset ys xs

-- that returns all the subsets of the set xs

subsets :: Set a -> [Set a]
subsets (Set []) = [Set []]
subsets (Set (x:xs)) = subsetsWithoutX ++ map (insertX x) subsetsWithoutX
  where
    subsetsWithoutX = subsets (Set xs)
    insertX a (Set ys) = Set (a:ys)



-- EXERCISE 6
-- Use a fold to create a predicate that given a list of lists of positive and negative integer numbers [xs], returns if the addition of any of those lists is positive.

anyPositiveSum :: [[Int]] -> Bool
anyPositiveSum = foldl (\ acc xs -> acc || sum xs > 0) False



main :: IO ()
main = do

    putStrLn "-- EXERCISE 1 --"
    putStrLn ""

    putStrLn $ "countr 1 [1, 2, 1, 1] = " ++ show (countr 1 [1, 2, 1, 1])
    putStrLn $ "countr 1 [] = " ++ show (countr 1 [])
    putStrLn $ "countr 1 [2, 3, 4, 5] = " ++ show (countr 1 [2, 3, 4, 5])
    putStrLn $ "countl 1 [1, 2, 1, 1] = " ++ show (countl 1 [1, 2, 1, 1])
    putStrLn $ "countl 1 [] = " ++ show (countl 1 [])
    putStrLn $ "countl 1 [2, 3, 4, 5] = " ++ show (countl 1 [2, 3, 4, 5])


    putStrLn ""
    putStrLn "-- EXERCISE 2 --"
    putStrLn ""

    putStrLn $ "allMax [1, 2, 3, 4] = " ++ show (allMax [1, 2, 3, 4])
    putStrLn $ "allMax [4, 3, 2, 1] = " ++ show (allMax [4, 3, 2, 1])
    putStrLn $ "allMax [] = " ++ show (allMax [])
    putStrLn $ "allMax [1, 2, 1, 3, 5, 4] = " ++ show (allMax [1, 2, 1, 3, 5, 4])


    putStrLn ""
    putStrLn "-- EXERCISE 3 --"
    putStrLn ""

    putStrLn $ "maxEvenStreak [0,1,2,3,4] = " ++ show (maxEvenStreak [0,1,2,3,4])
    putStrLn $ "maxEvenStreak [1,2,4,4] = " ++ show (maxEvenStreak [1,2,4,4])
    putStrLn $ "maxEvenStreak [1,2,2,3,4] = " ++ show (maxEvenStreak [1,2,2,3,4])
    putStrLn $ "maxEvenStreak [] = " ++ show (maxEvenStreak [])
    putStrLn $ "maxEvenStreak [1] = " ++ show (maxEvenStreak [1])


    putStrLn ""
    putStrLn "-- EXERCISE 4 --"
    putStrLn ""

    putStrLn $ "myAll even [1, 2, 3, 4] = " ++ show (myAll even [1, 2, 3, 4])
    putStrLn $ "myAny even [1, 2, 3, 4] = " ++ show (myAny even [1, 2, 3, 4])
    putStrLn $ "myAll (>3) [4, 5, 6, 7] = " ++ show (myAll (>3) [4, 5, 6, 7])
    putStrLn $ "myAll (<3) [4, 5, 6, 7] = " ++ show (myAll (<3) [4, 5, 6, 7])


    putStrLn ""
    putStrLn "-- EXERCISE 5 --"
    putStrLn ""

    let set1 = createSet [0,1,1]
    putStrLn $ "let set1 = createSet [0,1,1]: " ++ show set1
    let set2 = createSet [0]
    putStrLn $ "let set2 = createSet [0]: " ++ show set2

    putStrLn $ "subset set1 set2 = " ++ show (subset set1 set2)
    putStrLn $ "subset set2 set1 = " ++ show (subset set2 set1)
    putStrLn $ "subset set1 set1 = " ++ show (subset set1 set1)

    putStrLn $ "set1 == set1 = " ++ show (set1 == set1)
    putStrLn $ "set1 == set2 = " ++ show (set1 == set2)

    putStrLn $ "subsets set1 = " ++ show (subsets set1)
    putStrLn $ "subsets set2 = " ++ show (subsets set2)


    putStrLn ""
    putStrLn "-- EXERCISE 6 --"
    putStrLn ""

    putStrLn $ "anyPositiveSum [[0,0], [0, 0]] = " ++ show (anyPositiveSum [[0,0], [0, 0]])
    putStrLn $ "anyPositiveSum [[0,-1], [-1, -2], [0, 1, -2]] = " ++ show (anyPositiveSum [[0,-1], [-1, -2], [0, 1, -2]])
    putStrLn $ "anyPositiveSum [[0,-1], [-1, -2], [0, 1, -2, 2]] = " ++ show (anyPositiveSum [[0,-1], [-1, -2], [0, 1, -2, 2]])
    putStrLn $ "anyPositiveSum [[0,-1, 3], [-1, -2], [0, 1, -2]] = " ++ show (anyPositiveSum [[0,-1, 3], [-1, -2], [0, 1, -2]])
