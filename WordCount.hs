module WordCount where 
import Control.Arrow


main :: IO ()
main = do
   contents <- readFile "1453275352867.txt"
   let all = lines contents
   mapM putStrLn  $ getColumns (map (split ',') all) 0
   return ()
   
   
-- the same as (list !! index)
takeColumn :: [String] -> Int -> String
takeColumn [] _ = ""
takeColumn (x:xs) n
  | n == 0 = x
  | n >  0 = takeColumn xs (n-1)
  

getColumns :: [[String]] -> Int -> [String]
getColumns [] _ = []
getColumns (x:xs) n = (x !! n) : (getColumns xs n)

split:: Char -> String   -> [String]
split _ ""  = []
split c xs  = [h] ++ t
          where h = takeWhile (/=c) xs
                t = split c (tail $ dropWhile (/= c) xs) 
                
count :: String -> Kleisli IO String ()
count w = Kleisli readFile >>>
          arr words >>> arr (filter (/= w)) >>> arr (foldl (++) "") >>>
          Kleisli putStrLn
      
tuple2Int :: ([a],[b]) -> (Int,[b])
tuple2Int a@(x,y) = tuple2F a length

tuple2F :: ([a],[b]) ->([a]->c) -> (c,[b])
tuple2F (x,y) f = (f x ,y)

--first :: (a->c) ->(a,b) -> (c,b)
--first f (x,y)  = (f x ,y)

readInt :: String -> Int
readInt x = (read x)::Int

mydiv :: (Int,Int) -> Int
mydiv (x,y) =  quot x y

myDiv :: (Int,Int) -> Float
myDiv (a,b) = 
      let x = fromIntegral a
          y = fromIntegral b
      in  (x / y)


cross :: String -> Kleisli IO String ()
cross w = Kleisli readFile >>>
          --arr words >>> arr (\x -> (x,x)) >>> arr tuple2Int >>>
          --arr words >>> arr (\x -> (x,x)) >>> arr  (length *** (map read)) >>> arr (second sum) >>> arr myDiv >>>
          arr words 
          >>> arr (\x -> (x,x)) 
          >>> arr  (length *** (sum.(map read))) 
          >>> arr myDiv 
          >>> Kleisli print
          










