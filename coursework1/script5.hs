import Prelude hiding (or, minimum, lookup)
import Data.Char
import Text.Show

triangle :: Int -> Int
triangle 0 = 0
triangle x = x + triangle (x-1)

or :: [Bool] -> Bool
or xs = foldr (||) False xs

minimum :: [Int] -> Int
minimum (x:xs) = foldr min x xs

count :: Int -> [Int] -> Int
count n [] = 0
count n (x:xs) = if n == x then 1 + count n xs else 0 + count n xs

count1 n xs = foldr (\a -> if n == a then (1+) else id) 0 xs

--f = asdf

--foldr op e [] = e
--foldr op e (x:xs) = op x (foldr op e xs)


lookup :: Eq a => a -> [(a, b)] -> b
lookup n abs = b
    where [(_,b)] = filter (uncurry (\a _ -> a == n)) abs

euclid :: Int -> Int -> Int
euclid x y
    | y == 0    = x
    | otherwise = euclid y (x `mod` y)

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g (a:b:xs) = [f a, g b] ++ altMap f g xs
altMap f _ [x] = [f x]
altMap _ _ [] = []

luhn :: Show a => a -> Bool
luhn = ((==0) . (`mod` 10) . sum . altMap id luhnDouble . reverse . digits)

luhnDouble :: Int -> Int
luhnDouble x = if 2*x > 9 then 2*x-9 else 2*x

digits :: Show a => a -> [Int]
digits x = map digitToInt (show x :: [Char])