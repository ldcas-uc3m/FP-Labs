-- Lab 3: Lists
-- By Luis Daniel Casais Mezquida


-- EXERCISE 1
-- Define a function that puts the n first elements of a list at its end.
rotate :: (Num t, Eq t) => t -> [a] -> [a]
rotate 0 xs = xs  -- base case
rotate n (x:xs) = rotate (n-1) (xs ++ [x])  -- put the head to the end, rotate one less


-- EXERCISE 2
-- Define a function that returns the elements of xs between positions a and b (b not included).
interval :: (Num t, Eq t) => t -> t -> [a] -> [a]
interval 0 0 xs = []  -- base case
interval a 0 xs = []  -- if reaching b=0 before a=0 (a>b)
interval 0 b (x:xs) = x : interval 0 (b-1) xs  -- found first match, pop it and continue searching until the base case (the list is now 1 element smaller)
interval a b (x:xs) = interval (a-1) (b-1) xs  -- discard first element, continue searching (the list is now 1 element smaller)


-- EXERCISE 3
-- Given a two lists of 2 elements [a,b] and [c,d] that represent the boundaries of two intervals define a function that returns the intersection of both intervals.
intersection :: (Eq a, Enum a) => [a] -> [a] -> [a]
intersection [] _ = []  -- base case
intersection _ [] = []  -- base case
intersection [a,b] [c,d] = filter (`elem` xs) ys  -- filter the ones from ys that are from xs
    where xs = [a..b]
          ys = [c..d]


-- EXERCISE 4
-- Define a function that returns the addition of the elements in even positions in a list of integer numbers.
addEven :: [Integer] -> Integer
-- all lists will always start by and odd element
addEven [] = 0  -- base case 0
addEven [x] = 0  -- base case 1
addEven (x:[y]) = y  -- base case 2
addEven (x:y:xs) = y + addEven xs  -- pop 1st, save 2nd, continue w/ 3rd (starts w/ odd element)


-- EXERCISE 5
-- Define a function that behaves like the take function but if n >= length xs, the last element of the list is repeated until the returned list has n elements. For the empty list it must return the empty list. Make sure it works for infinite lists.

takeN :: (Eq t, Num t) => t -> [a] -> [a]
takeN n [] = []  -- special case
takeN 0 xs = []  -- base case
takeN n [x] = x : takeN (n-1) [x]  -- repeat last element (infinite-friendly)
takeN n (x:xs) = x : takeN (n-1) xs



-- EXERCISE 6
-- Create a function that given a lower case string returns a tuple with the (highest character, smallest character) considering the Spanish alphabet (both a and á, etc. must be considered, ñ appears in between n and o). Assume the string will contain at least one lower case letter and it will not contain anything that it is not a lower case letter.

tildeToVowel :: Char -> Char
-- convert vowels with tildes to vowels without tildes
tildeToVowel c  -- translation
    | c == 'á' = 'a'
    | c == 'é' = 'e'
    | c == 'í' = 'i'
    | c == 'ó' = 'o'
    | c == 'ú' = 'u'
    | otherwise = c

max' :: Char -> Char -> Char
-- max function taking into account ñ
max' x y
    | x == 'ñ' && y <= 'n' = x
    | x == 'ñ' && y >= 'o' = y
    | otherwise = max x y

maximum' :: [Char] -> Char
-- maximum function taking into account ñ
maximum' [x] = x
maximum' (x:[y]) = max' x y
maximum' (x:cs) = max' x (maximum' cs)

min' :: Char -> Char -> Char
-- min function taking into account ñ
min' x y
    | x == 'ñ' && y <= 'n' = y
    | x == 'ñ' && y >= 'o' = x
    | otherwise = min x y

minimum' :: [Char] -> Char
-- minimum function taking into account ñ
minimum' [x] = x
minimum' (x:[y]) = min' x y
minimum' (x:cs) = min' x (minimum' cs)


maxMinLetter :: [Char] -> (Char, Char)
maxMinLetter cs = (maximum' (map tildeToVowel cs), minimum' (map tildeToVowel cs))



main :: IO ()
main = do

    putStrLn "-- EXERCISE 1 --"
    putStrLn ""

    putStrLn ("rotate 1 [1..7]: " ++ show (rotate 1 [1..7]))
    putStrLn ("rotate 2 [1..7]: " ++ show (rotate 2 [1..7]))
    putStrLn ("rotate 3 [1..7]: " ++ show (rotate 3 [1..7]))
    putStrLn ("rotate 7 [1..7]: " ++ show (rotate 7 [1..7]))
    putStrLn ("rotate 8 [1..7]: " ++ show (rotate 8 [1..7]))

    putStrLn ""
    putStrLn "-- EXERCISE 2 --"
    putStrLn ""

    putStrLn ("interval 0 1 [0..7]: " ++ show (interval 0 1 [0..7]))
    putStrLn ("interval 0 6 [0..7]: " ++ show (interval 0 6 [0..7]))
    putStrLn ("interval 2 5 [0..7]: " ++ show (interval 2 5 [0..7]))
    putStrLn ("interval 4 2 [0..7]: " ++ show (interval 4 2 [0..7]))

    putStrLn ""
    putStrLn "-- EXERCISE 3 --"
    putStrLn ""

    putStrLn("intersection [] [3,5]: " ++ show (intersection [] [3,5]))
    putStrLn("intersection [3,5] []: " ++ show (intersection [3,5] []))
    putStrLn("intersection [2,4] [6,9]: " ++ show (intersection [2,4] [6,9]))
    putStrLn("intersection [2,6] [6,9]: " ++ show (intersection [2,6] [6,9]))
    putStrLn("intersection [2,6] [0,9]: " ++ show (intersection [2,6] [0,9]))
    putStrLn("intersection [2,6] [0,4]: " ++ show (intersection [2,6] [0,4]))
    putStrLn("intersection [4,6] [4,4]: " ++ show (intersection [4,6] [4,4]))
    putStrLn("intersection [5,6] [0,4]: " ++ show (intersection [5,6] [0,4]))

    putStrLn ""
    putStrLn "-- EXERCISE 4 --"
    putStrLn ""

    putStrLn("addEven []: " ++ show (addEven []))
    putStrLn("addEven [1]: " ++ show (addEven [1]))
    putStrLn("addEven [1..4]: " ++ show (addEven [1..4]))
    putStrLn("addEven [1..7]: " ++ show (addEven [1..7]))

    putStrLn ""
    putStrLn "-- EXERCISE 5 --"
    putStrLn ""

    putStrLn("takeN 3 [1,2,3]: " ++ show (takeN 3 [1,2,3]))
    putStrLn("takeN 4 [1,2]: " ++ show (takeN 4 [1,2]))
    putStrLn("takeN 5 [1]: " ++ show (takeN 5 [1]))
    putStrLn("takeN 8 \"hello\": " ++ show (takeN 8 "hello"))
    -- putStrLn("takeN 4 []: " ++ show (takeN 4 []))  -- no funciona el show porque patata
    putStrLn("takeN 22 (repeat \"h\"): " ++ show (takeN 22 (repeat "h")))

    putStrLn ""
    putStrLn "-- EXERCISE 6 --"
    putStrLn ""

    putStrLn("maxMinLetter \"hello\": " ++ show (maxMinLetter "hello"))
    putStrLn("maxMinLetter \"ñandú\": " ++ show (maxMinLetter "ñandú"))
    putStrLn("maxMinLetter \"niña\": " ++ show (maxMinLetter "niña"))