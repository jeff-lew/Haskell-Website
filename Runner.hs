import Data.Maybe

hailstone :: (Integral a, Num a) => a -> a
hailstone x 
    | even x        = div x 2
    | otherwise     = x * 3 + 1

hailSeq :: (Integral a, Num a) => a -> [a]
hailSeq x = generateHailSeqRec [] x
    where
        generateHailSeqRec :: (Integral a, Num a) => [a] -> a -> [a]
        generateHailSeqRec seq 1  = seq ++ [1]
        generateHailSeqRec seq x  = generateHailSeqRec (seq ++ [x]) (hailstone x)

hailSeq' :: (Integral a, Num a) => a -> [a]
hailSeq' x = (takeWhile (>1) (hailstoneSeq x)) ++ [1]
    where
        hailstoneSeq :: (Integral a) => a -> [a]
        hailstoneSeq x = iterate hailstone x

join :: [a] -> [[a]] -> [a]
join link [] = []
join link xs = foldl1 (myJoin link) xs
    where
        myJoin :: [a] -> [a] -> [a] -> [a]
        myJoin link x y = x ++ link ++ y

merge :: Ord a => [a] -> [a] -> [a]
merge xs ys = recurseMerge [] xs ys
    where 
        recurseMerge :: Ord a => [a] -> [a] -> [a] -> [a]
        recurseMerge acc [] ys = acc ++ ys
        recurseMerge acc xs [] = acc ++ xs
        recurseMerge acc (x:xs) (y:ys)
            | x < y     = recurseMerge (acc ++ [x]) xs (y:ys)
            | otherwise = recurseMerge (acc ++ [y]) (x:xs) (ys)

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []   
mergeSort (x:[]) = merge [x] []     -- [1] in [1,2,3] -> [1] [2,3]
mergeSort (x:y:[]) = merge [x] [y]  -- [1,2]
mergeSort xs = merge (mergeSort firstHalf) (mergeSort secondHalf)
    where
        n = div (length xs) 2
        firstHalf = take n xs
        secondHalf = drop n xs

-- findElt :: Eq a => a -> [a] -> Maybe (a, Integer)
-- findElt elem list = indexOf (isFound elem list)
--     where 
--         isFound :: Eq a => a -> [a] -> Maybe (a, Integer)
--         isFound elem list = listToMaybe [ (x, i) | (x, i) <- zip list [0..], x == elem ]
--         indexOf :: Maybe (a, Integer) -> Maybe (a, Integer)
--         indexOf isFound = case isFound of
--             Nothing -> Nothing
--             Just isFound -> Just isFound

findElt :: Eq a => a -> [a] -> Maybe Integer
findElt elem list = indexOf (isFound elem list)
    where 
        isFound :: Eq a => a -> [a] -> Maybe Integer
        isFound elem list = listToMaybe [ i | (x, i) <- zip list [0..], x == elem ]
        indexOf :: Maybe Integer -> Maybe Integer
        indexOf isFound = case isFound of
            Nothing -> Nothing
            Just isFound -> Just isFound