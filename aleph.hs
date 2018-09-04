head' :: [a] -> a
head' [] = error "nah bro there isn't even like one thing in there dude"
head' (x:_ ) = x


mergesort :: (Ord a) => [a] -> [a]
mergesort [x] = [x]
mergesort (x:xs) = merge [x] (mergesort xs)


merge :: (Ord a) => [a] -> [a] -> [a]
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys
merge [] [] = []
merge x [] = x
merge [] y = y


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerOrEqual = [a | a <- xs, a <= x]
        larger = [a | a <- xs, a > x]
    in quicksort smallerOrEqual ++ [x] ++ quicksort larger


fib :: (Integral a) => a -> a
fib 0 = 1
fib 1 = 1
fib n
    | n >= 2 = (fib (n - 1)) + (fib (n - 2))
    | otherwise = error "sounds like that's a negative number you got right there"


reverse' :: [a] -> [a]
reverse' [] = []
reverse' [x] = [x]
reverse' (x:xs) = (reverse' xs) ++ [x]

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x = x : filter' p xs
    | otherwise = filter' p xs


collatz :: Integer -> [Integer]
collatz n
    | n == 1 = [1]
    | even n = n : collatz (div n 2)
    | odd n = n : collatz (n*3+1)

numLongChains x = length (filter (\xs -> length xs > 15) (map collatz x))

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs

dotprime :: (b -> c) -> (a -> b) -> a -> c
dotprime f g = \x -> f $ g x
