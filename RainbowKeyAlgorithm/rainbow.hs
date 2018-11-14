import RainbowAssign
import System.Random
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

---------------- Declarations ----------------

pwLength, nLetters, width, height :: Int
filename :: FilePath
pwLength = 8            -- length of each password
nLetters = 5            -- number of letters to use in passwords: 5 -> a-e
width = 40              -- length of each chain in the table
height = 1000           -- number of "rows" in the table

filename = "table.txt"  -- filename to store the table

-- pwLength = 5
-- nLetters = 18
-- width = 60
-- height = 800

-- table = rainbowTable 40 ["abcdeabc", "aabbccdd", "eeeeeeee"]
-- table = rainbowTable width ["acdgcddh","fcfeggeh","ebfeecbe"]
--table = rainbowTable width ["aljdm","dlkhg","madnh","fcdfo","lndof"]


---------------- Helper Functions ----------------

toInt :: Hash -> Int
toInt = fromIntegral

nextHash :: Hash -> Hash
nextHash hash = pwHash (pwReduce hash)

nextPwd :: Passwd -> Passwd
nextPwd pwd = pwReduce (pwHash pwd)


---------------- Defined Functions ----------------

pwReduce :: Hash -> Passwd
pwReduce hash32 = map toLetter (reverseAndTake (convertBase intHash nLetters pwLength) pwLength)
    where
        convertBase :: Int -> Int -> Int -> [Int]
        convertBase intHash base length
            | length == 0 = []
            | otherwise = (mod intHash base) : (convertBase (div intHash base) base (length - 1))
        reverseAndTake :: [Int] -> Int -> [Int]
        reverseAndTake arr length = reverse (take length arr)
        intHash = toInt hash32

rainbowTable :: Int -> [Passwd] -> Map.Map Hash Passwd
rainbowTable inputWidth passwords = (Map.fromList [ ((generateMapping inputWidth pwd), pwd) | pwd <- passwords ])
    where
        generateMapping :: Int -> Passwd -> Hash
        generateMapping 0 pwd = pwHash pwd
        generateMapping width pwd = generateMapping (width - 1) (nextPwd pwd)

findPassword :: Map.Map Hash Passwd -> Int -> Hash -> Maybe Passwd
findPassword table inputWidth hash = reduceList (listOfMaybes) ((length (listOfMaybes)) - 1)
    where
        listOfMaybes = generateListOfMaybes hash inputWidth
        generateListOfMaybes :: Hash -> Int -> [Maybe Passwd]
        generateListOfMaybes _ (-1) = [Nothing]
        generateListOfMaybes hash width = case (searchTable hash) of
            Just password -> (findPwd width password) : (generateListOfMaybes (nextHash hash) (width - 1))
            Nothing -> generateListOfMaybes (nextHash hash) (width - 1)
        searchTable :: Hash -> Maybe Passwd
        searchTable hash = Map.lookup hash table
        findPwd :: Int -> Passwd -> Maybe Passwd
        findPwd (-1) pwd
            | (pwHash pwd) == hash = Just pwd
            | otherwise = Nothing
        findPwd width pwd
            | (pwHash pwd) == hash = Just pwd
            | otherwise = findPwd (width - 1) (nextPwd pwd)
        reduceList :: [Maybe Passwd] -> Int -> Maybe Passwd
        reduceList _ (-1) = Nothing 
        reduceList list index = case (list !! index) of
            Just pwd -> Just pwd
            Nothing -> reduceList (list) (index - 1)

---------------- Other ----------------

generateTable :: IO ()
generateTable = do
    table <- buildTable rainbowTable nLetters pwLength width height
    writeTable table filename

test1 = do
    table <- readTable filename
    return (Map.lookup (-2144413882) table)

test2 :: Int -> IO ([Passwd], Int)
test2 n = do
  table <- readTable filename
  pws <- randomPasswords nLetters pwLength n
  let hs = map pwHash pws
  let result = Maybe.mapMaybe (findPassword table width) hs
  return (result, length result)


---------------- Main ----------------
main :: IO ()
main = do
    generateTable
    res <- test2 1000
    print res