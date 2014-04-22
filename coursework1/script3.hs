import Data.Char
import Text.Show

verbose :: Bool -> Bool
verbose b = b

complex :: x -> y -> (Int, y)
complex x y = (2, y)

xor1 :: Bool -> Bool -> Bool
xor1 b False = b
xor1 False b = b
xor1 True True = False

--xor2 :: Bool -> Bool -> Bool
--xor2 a b = if  then False else True

xor3 :: Bool -> Bool -> Bool
xor3 a b = a /= b

third1 :: [a] -> a
third1 xs = head (tail (tail xs))

third2 :: [a] -> a
third2 (_:(_:(x:_))) = x

third3 :: [a] -> a
third3 xs = xs!!2

digs :: Integer -> [Integer]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

luhnDouble :: Int -> Int
luhnDouble x = if 2*x > 9 then 2*x-9 else 2*x

luhnList :: [Int] -> [Int]
luhnList (a:(b:xs)) = [luhnDouble a] ++ [b] ++ luhnList xs
luhnList (a:x) = [luhnDouble a] ++ x
luhnList x = x

digits :: Show a => a -> [Int]
digits x = map digitToInt (show x :: [Char])

luhn :: Show a => a -> Bool
luhn a = (sum (luhnList (reverse (digits a)))) `mod` 10 == 0

luhnUgly :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool
luhnUgly a b c d e f g h i j k l m n o p = (luhnDouble a + b + luhnDouble c + d + luhnDouble e + f + luhnDouble g + h + luhnDouble i + j + luhnDouble k + l + luhnDouble m + n + luhnDouble o + p) `mod` 10 == 0
