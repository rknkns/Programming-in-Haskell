import Data.Char

add :: (Int, Int) -> Int
add (x,y) = x + y

zeroto :: Int -> [Int]
zeroto n = [0..n]

add' :: Int -> (Int -> Int)
add' x y = x + y

---mult :: Int -> (Int -> (Int -> Int))

copy' :: a -> (a, a)
copy' x = (x,x)

itself :: a -> a
itself x = x

--- 1. ---['a', 'b', 'c'] :: [Char]
   ---('a', 'b', 'c') :: (Char, Char, Char)
   ---[(False, '0'), (True, '1')] :: [(Bool, Int), (Bool, Int)]
   ---([False, True], ['0', '1']) :: ([Bool], [Int])
   ---[tail, init, reverse] :: [[a] -> [a]]

bools :: [Bool]
bools = [True, False, False]

nums :: [[Int]]
nums = [[2,3], [4,5,6], [1,2,3]]

add1 :: Int -> Int -> Int -> Int
add1 x y z = x + y + z

copy :: a -> (a, a)
copy x = (x, x)

apply :: (a -> b) -> a -> b
apply g x = g x

second :: [a] -> a
second xs = head (tail xs)

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

pair :: a -> b -> (a, b)
pair x y = (x, y)

double :: Num a  => a -> a
double x = x * 2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs


twice :: (a -> a) -> a -> a
twice f x = f (f x)

even' :: Integral a => a -> Bool
even' n = n `mod` 2 == 0

splitat :: Int -> [a] -> ([a], [a])
splitat n xs = (take n xs, drop n xs)

reciprocal :: Fractional a => a -> a
reciprocal n = 1/n

abs' :: Int -> Int
abs' n = if n >= 0 then n else -n

signum' :: Int -> Int
signum' n = if n > 0 then 1 else if n == 0 then 0 else -1

abs1 :: Int -> Int
abs1 n | n >= 0    = n
       | otherwise = -n

signum1 :: Int -> Int
signum1 n | n > 0     = 1
          | n == 0     = 0
          | otherwise = -1

test :: [Char] -> Bool
test ['a', _, _] = True
test _           = False

test1 :: [Char] -> Bool
test1 ('a': _) = True
test1 _        = False

head1 :: [a] -> a
head1 (x:_) = x

tail1 :: [a] -> [a]
tail1 (_:xs) = xs

add2 :: Int -> (Int -> Int)
add2 = \x -> (\y -> x + y)

--const' :: a -> (b -> a)
--const' x = \_ -> x

const' :: a -> b -> a
const' x _ = x

--odds :: Int -> [Int]
--odds n = map f [0..n-1]
         where f x = x * 2 + 1

odds :: Int -> [Int]
odds n = map (\x -> x*2 + 1) [0..n-1]

length' :: [a] -> Int
length' xs = sum (map (const' 1) xs)

trd :: (a, b, c) -> c
trd (_, _, x) = x

thesame :: a -> (b -> a)
thesame n = \_ -> n

onemore :: [Int] -> [Int]
onemore xs = map (\x -> x + 1) (xs)

plusone :: [Int] -> [Int]
plusone xs = map (+1) (xs)

---EXERCISES---
---ONE!---
halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
         where n = (length xs) `div` 2

--- TWO!!---
--a--
third :: [a] -> a
third xs = head (tail (tail xs))

--b--
third2 :: [a] -> a
third2 xs = xs !! 2

--c--
third3 :: [a] -> a
third3 (_: _: a: xs) = a

---THREE!!!---
--a--
safetail1 :: [a] -> [a]
safetail1 xs = if null xs == False then tail xs else []

--b--
safetail2 :: [a] -> [a]
safetail2 xs | null xs == False  = tail xs
             | otherwise         = []

--c--
safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 (_:xs) = xs

---FOUR!!!!---
disjunction1 :: Bool -> Bool -> Bool
disjunction1 a b | a == b    = a
                 | otherwise = False

disjunction2 :: Bool -> Bool -> Bool
disjunction2 a b | a == True      = b
                 | otherwise      = False

disjunction3 a b | a b == True   = True
                 | otherwise     = False

disjunction4 a b | a == False    = False
                 | b == False    = False
                 | otherwise     = True

---FAHVE!!!!!---
logconjunction :: Bool -> Bool -> Bool
logconjunction a b = if a then b else a

---SIX!!!!!!---
logconjunction' :: Bool -> Bool -> Bool
logconjunction' a b = if a then b else False

---SEVEN!!!!!!!---
mult :: Int -> Int -> Int -> Int
mult = \x -> (\y -> (\z -> x*y*z))

---EIGHT!!!!!!!!---
luhnDouble :: Int ->  Int
luhnDouble x = if y <= 9 then y else y - 9
               where y = x * 2


luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = ((luhnDouble a) + b + (luhnDouble c) + d) `mod` 10 == 0

concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs]

firsts' :: [(a,b)] -> [a]
firsts' xs = [x | (x,_) <- xs]

length'' :: [a] -> Int
length'' xs = sum [1| _ <- xs ]

factors :: Int -> [Int]
factors x = [y | y <- [1..x], x `mod` y == 0]

prime :: Int -> Bool
prime x = factors x == [1,x]

primes :: Int -> [Int]
primes x = [y | y <- [1..x], prime y]

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k' == k]

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']

lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n ) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [ shift n x | x <- xs]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) (lowers xs) | x <- xs]

freqs' :: String -> [Float]
freqs' xs = [percent (count x xs) n | x <- ['a'..'z']]
            where n = lowers xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e) ^ 2) / e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

table' :: [Float]
table' = freqs' "kdvnhoo lv ixq"

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

crack :: String -> String
crack xs = encode (-factor) xs
      where
        factor = head (positions (minimum chitab) chitab)
        chitab = [chisqr (rotate n table'') table | n <- [0..25]]
        table'' = freqs' xs

--EXERCISES--
--One!--
exeone :: Int -> Int
exeone n = sum [x ^ 2 | x <- [1..n]]

--Two!!--
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

--Three!!!--
square :: Int -> [(Int, Int)]
square n = [(x, y) | x <- [0..n], y <- [0..n], x /= y]

--Four!!!!--
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x

replicate'' :: Int -> a -> [a]
replicate'' n x = [x | _ <- [1..n]]

---Five !!!!!---
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], z ^ 2 == x ^ 2 + y ^ 2]

---Six!!!!!!---
perfects :: Int -> [Int]
perfects n = [y | y <- [1..n], (sum (factors y)) == y * 2]
