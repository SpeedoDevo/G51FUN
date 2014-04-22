import Data.Char

evens :: [Int] -> [Int]
evens xs = filter even xs

squares :: Int -> [Int]
squares x = [x^2 | x <- [1..x]]

fiftySquares :: Int
fiftySquares = sum (squares 50)

coords :: Int -> Int -> [(Int,Int)]
coords m n = [(x,y) | x <- [0..m], y <- [0..n]]

noDiagonal :: Int -> [(Int,Int)]
noDiagonal n = filter (uncurry (/=)) (coords n n)

clean :: String -> String
clean s =  filter isAlpha (map toLower s)