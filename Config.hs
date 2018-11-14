-- import System.Random

-- threeRand' :: IO [Int]
-- threeRand' = do
--     gen0 <- newStdGen
--     let
--         (rand0, gen1) = randomR (1, 100) gen0
--         (rand1, gen2) = randomR (1, 100) gen1
--         (rand2, _)    = randomR (1, 100) gen2
--     return [rand0, rand1, rand2]

-- returnGetLine = do
--     l1 <- getLine
--     return l1

myIterate :: Enum a => (a -> a) -> a -> [a]
myIterate f a = [a, (f a)..]

-- myTakeWhile f a = foldl (genMyTakeWhile) [] a
--     where 
--         genMyTakeWhile l r 
--             | f (r) == True = l ++ [r]
--             | otherwise = l

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile f a = foldr (genMyTakeWhile f) [] a
    where 
        genMyTakeWhile :: (a -> Bool) -> a -> [a] -> [a]
        genMyTakeWhile f l r
            | f (l) == True = [l] ++ r
            | otherwise = []

pascal :: (Eq a, Num a) => a -> [a]
pascal n = genPascalSeq n 
    where 
        genPascalSeq :: (Eq a, Num a) => a -> [a]
        genPascalSeq 0 = [1]
        genPascalSeq 1 = [1,1]
        genPascalSeq n = [1] ++ [ i+j | (i,j) <- zip (genPascalSeq (n-1)) (tail(genPascalSeq (n-1))) ] ++ [1]

addPair :: (Num a) => (a, a) -> a
addPair = uncurry (+)

withoutZeros :: (Eq a, Num a) => [a] -> [a]
withoutZeros = foldl (isZero) []
    where
        isZero :: (Eq a, Num a) => [a] -> a -> [a]
        isZero l r
            | r == 0 = l
            | otherwise = l ++ [r]

fib :: (Eq a, Num a) => a -> a
fib n = snd(generateFib n)
    where
        generateFib :: (Eq a, Num a) => a -> (a,a)
        generateFib 0 = (0,0)
        generateFib 1 = (0,1)
        generateFib n = (snd(generateFib (n-1)), uncurry (+) (generateFib (n-1)))

-- fib2 n = snd(generateFib n)
--     where
--         generateFib 0 = (0,0)
--         generateFib 1 = (0,1)
--         generateFib n = (snd(prevFibSet n), uncurry (+) (prevFibSet n))
--         prevFibSet n = generateFib (n-1)

-- fibSequence n = generateFibSequence n
--     where 
--         generateFibSequence 0 = [0]
--         generateFibSequence 1 = [0,1]
--         generateFibSequence n = (seq n) ++ [(seq n)!!((length (seq n)) - 1) + (seq n)!!((length (seq n)) - 2)]
--         seq  n = (generateFibSequence (n-1))
        
things :: [Integer]
things = 0 : 1 : zipWith (+) things (tail things)


-- addPairNonTuple = \x y -> x + y
-- addTenLambda = \x -> x + 10
-- addFive = (+ 5) 
