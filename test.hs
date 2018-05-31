double x = x + x
quadruple x = double (double x)

factorial n = product [1..n]
average ns = sum ns `div` length ns

n = a `div` length xs
    where
      a = 10
      xs = [1,2,3,4,5]

f xs = head (drop n xs)
     where n = (length xs) - 1

g xs = reverse (tail (reverse xs))

h xs = head (reverse xs)
j xs = take n xs
     where
       n = (length xs) - 1
        
