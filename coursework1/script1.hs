double x = x+x
quadruple x = double (double x)
smallest x y = if x < y then x else y
largest x y = if x > y then x else y
diff x y = l - s
    where
        l = largest x y
        s = smallest x y
factorial n = product [1..n]
average ns = sum ns `div` length ns
lastf list = list !! n
    where
        n = length list - 1
initf list = take n list
    where
        n = length list - 1