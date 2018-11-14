import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate

merge :: Ord a => [a] -> [a] -> [a]
merge xs ys = recurseMerge [] xs ys
    where 
        recurseMerge :: Ord a => [a] -> [a] -> [a] -> [a]
        recurseMerge acc [] ys = acc ++ ys
        recurseMerge acc xs [] = acc ++ xs
        recurseMerge acc (x:xs) (y:ys)
            | x < y     = recurseMerge (acc ++ [x]) xs (y:ys)
            | otherwise = recurseMerge (acc ++ [y]) (x:xs) (ys)

-- My original hailLen is tail-recursive
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

hailLen' :: (Integral a, Num a) => a -> a
hailLen' x = calculateHailLen x
    where
        calculateHailLen :: (Integral a, Num a) => a -> a
        calculateHailLen 1 = 0
        calculateHailLen x = calculateHailLen (hailstone x) + 1

fact :: (Num a, Eq a) => a -> a
fact x = calculateFactorial 1 x
    where 
        calculateFactorial :: (Num a, Eq a) => a -> a -> a
        calculateFactorial acc 0 = acc
        calculateFactorial acc x = calculateFactorial (acc*x) (x-1)

fact' :: (Enum a, Num a) => a -> a
fact' x = foldl (*) 1 [1..x]

daysInYear :: Integer -> [Day]
daysInYear y = [jan1..dec31]
    where 
        jan1 = fromGregorian y 1 1
        dec31 = fromGregorian y 12 31

isFriday :: Day -> Bool
isFriday date 
    | snd (mondayStartWeek date) == 5 = True
    | otherwise     = False

getDay :: (year,month,day) -> day
getDay (_,_,d) = d

divisors :: Int -> [Int]
divisors n = [ i | i <- [2..(n `div` 2)], n `mod` i == 0 ]

isPrimeDay :: Day -> Bool
isPrimeDay date
    | divisors (getDay (toGregorian date)) == [] = True
    | otherwise     = False

primeFridays :: Integer -> [Day]
primeFridays year = [ i | i <- (daysInYear year), isFriday i && isPrimeDay i ] 
