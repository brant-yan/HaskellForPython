module ExerciseA where

import System.Random

main1 ::  IO () 
main1 = print "Hello World"


main2:: IO ()
main2 = do
    print "Please input your name"
    name <- getLine
    print ("Hello "++ name)

main3 :: IO ()
main3 = do
    print "Please input your name"
    name <- getLine
    case (elem name ["Alice","Bob"]) of
        True -> print ("Hello "++ name)
        False -> print "Bye"

main4 :: IO ()
main4 = do
    print "Please input a number"
    num <- readLn  :: IO Int
    print $ sum [1 .. num]

main5 :: IO ()
main5 = do
    print "Please input a number"
    num <- readLn  :: IO Int
    print $ sum  $ filter (\x -> (x `mod` 3 == 0) || (x `mod` 5 == 0)) [1 .. num]

main6 :: IO ()
main6 = do
    print "Please input a number"
    num <- readLn  :: IO Int
    seed  <- getStdGen
    case (fst $ next seed) > 0 of
        True -> print $ sum  $ filter (\x -> (x `mod` 3 == 0) || (x `mod` 5 == 0)) [1 .. num]
        False -> print $ product  [1 .. num]

{-
makeLine ::  Int -> [Int] -> String
makeLine  x [] = "\n"
makeLine x (y:ys) = (show (x * y)) ++ ['\t'] ++  makeLine x ys

makeLines :: Int -> [String]
makeLines n
    |  (n > 0 && n < 13) =   (makeLines (n - 1)) ++ [(makeLine n [1..n])]
    | otherwise = []


main7 :: IO ()
main7 = do
    let t  = runTable (Table makeLines) 12
    mapM print t
    return ()
data Table = Table {runTable :: Int -> [String]}
-}

main7 :: IO ()
main7 = do
        let table = createTable 12
        print $ stringForTable table
        return ()

data Table = Table {row :: [Row]}
data Row = Row {node::[Node]}
type Node = String 
type RowIndex = Int
type ColumnIndex = Int 

stringForTable :: Table -> String
stringForTable (Table rows) = foldl (\x y -> x ++"\n"++ y) "" (map stringForRow rows)

stringForRow:: Row -> String
stringForRow (Row nodes) = foldl (\x y -> x ++ "\t" ++ y) "" nodes


createTable :: Int -> Table
createTable n = Table [createRow ri 12 | ri <- [1 .. n ]]
                where
                    createRow ri ci = Row [createNode ri cii| cii <- [1..ci] ]
                    createNode i j = (show i) ++ "*" ++ (show j)

main8:: IO ()
main8 = do
        let maxNumber = 1000
        let primes = filter (\(x,y)-> y ==True ) $ zip [2..maxNumber]  (map isPrime [2..maxNumber])
        mapM (print.fst) primes
        return ()
isPrime :: Int -> Bool
isPrime n
    | n == 2  = True
    | n == 3  = True
    | n <=1   = False
    | otherwise = all (\x -> x && True) [(n `mod` nn ) > 0| nn <- [2..(n-1)], (isPrime nn)   ]

{-
data CMP = LT | EQ | GT deriving (Eq,Order,Show)
compareNumb :: Int -> Int -> CMP
compareNumb a b
    | b < a = LT
    | b > a = GT
    | b == a = EQ
-}
example ::(Num a)=> (a->a->a) -> a -> a
example f x  = (f 3 4) + x

everything :: [Bool] -> Bool
everything list = case list of
                        [] -> False
                        (x:xs) -> x && everything xs

func :: (a->b) -> IO a -> IO b
func  f  k = do
            kk <- k            
            return (f kk)
readInt :: String -> Int
readInt s = read s