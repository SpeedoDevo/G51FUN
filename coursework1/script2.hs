e1 :: [Bool]
e1 = [False,True,False]

e2 :: [[Int]]
e2 = [[1,2],[3,4]]

e3 :: (Char,Bool)
e3 = ('a',True)

e4 :: [(Char,Int)]
e4 = [('a',1),('b',2)]

e5 :: Float -> Float
e5 x = x * 2

e6 :: Int -> Int -> Int
e6 x y = x/y

e7 :: (a,b) -> a
e7 (x,y) = x

e8 :: ([Char],[Float])
e8 = (['a','b','c'],[3.1415,2.7182])

e9 :: a -> (a,a)
e9 x = (x,x)