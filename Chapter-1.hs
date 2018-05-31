double x = x + x

qsort [] = []
qsort (x:xs) = qsort larger ++ [x] ++ qsort smaller
               where
                 larger = [a | a <- xs, a > x]
                 smaller = [b | b <- xs, b < x]

product2 [] = 1
product2 (x:xs) = x * product2 xs

quadruple x = double (double x)
