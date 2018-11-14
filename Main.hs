det a b c = b^2 - 4*a*c
quadsol1 a b c = (-b - sqrt (det a b c))/2*a
quadsol2 a b c = (-b + sqrt (det a b c))/2*a

third_a (xs) = xs!!2
third_b (_:_:x:xs) = x

hailstone :: (Integral a, Num a) => a -> a
hailstone x 
    | even x        = div x 2
    | otherwise     = x * 3 + 1

hailLen :: (Integral a, Num a) => a -> a 
hailLen x = calculateHailLen 0 x
    where
        calculateHailLen :: (Integral a, Num a) => a -> a -> a
        calculateHailLen acc 1  = acc
        calculateHailLen acc x  = calculateHailLen (acc+1) (hailstone x)

divisors :: Int -> [Int]
divisors n = [ i | i <- [2..(n `div` 2)], n `mod` i == 0 ]

primes :: Int -> [Int]
primes n = [ i | i <- [2..n], divisors i == [] ]

join :: [a] -> [[a]] -> [a]
join link [] = []
join link (x:[]) = x
join link (x:xs) = x ++ link ++ join link xs

join' :: [a] -> [[a]] -> [a]
join' link [] = []
join' link (x:[]) = x
join' link (x:xs) = joinHelper' x link xs
    where 
        joinHelper' :: [a] -> [a] -> [[a]] -> [a]
        joinHelper' acc link [] = acc
        joinHelper' acc link (x:xs) = joinHelper' (acc ++ link ++ x) link xs

pythagorean :: Int -> [(Int, Int, Int)] 
pythagorean n = [ (a, b, c) | 
    a <- [1..n], 
    b <- [a+1..n],
    c <- [a+2..n], 
    a*a + b*b == c*c
    ]

pythagorean' :: Int -> [(Int, Int, Int)] 
pythagorean' n = [ (b, a, c) | 
    c <- [3..n],
    b <- [1..n], 
    a <- [b+1..n],
    a*a + b*b == c*c
    ]
