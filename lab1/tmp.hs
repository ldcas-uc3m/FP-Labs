import Data.Char

capitalise :: Char -> Char
capitalise x
    | (x >= 'a') && (x <= 'z') = chr (ord x - delta)
    | otherwise = x
    where delta = ord 'a' - ord 'A'

main :: IO ()
main = do
    print (capitalise 'a')