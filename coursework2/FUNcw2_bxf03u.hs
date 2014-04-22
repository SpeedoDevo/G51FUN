import Data.List (sort)
type Party = String
type Ballot = [Party]

count :: Eq a => a -> [a] -> Int
count n xs = foldr (\x -> if n == x then (1+) else (0+)) 0 xs

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (\nx -> (x /= nx)) xs)

frequency :: Eq a => [a] -> [(Int,a)]
frequency xs = [(count x xs,x) | x <- rmdups xs]

results :: [Party] -> [(Int,Party)]
results = sort . frequency

votes :: [Party]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

winner :: [Party] -> Party
winner = snd . last . results

-----------------------------------------------------------------

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter ([]/=)

remove :: Eq a => a -> [[a]] -> [[a]]
remove r = map $ filter (r/=)

ballots :: [Ballot]
ballots = [b1, b2, b3, b4, b5, b6]
b1 = ["Blue", "Green"]
b2 = ["Green", "Blue", "Red"]
b3 = ["Blue"]
b4 = ["Red", "Green"]
b5 = ["Blue", "Red", "Green"]
b6 = ["Green", "Red"]

rank :: [Ballot] -> [Party]
rank = map snd . results . map head

election bs = case rank bs of
                   [p] -> p
                   (p:ps) -> election . rmempty $ remove p bs