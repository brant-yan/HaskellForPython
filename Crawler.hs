{-# LANGUAGE OverloadedStrings #-}
module Crawler where
  

import Network.Wreq
import Control.Lens
import Data.Aeson.Lens (_String, key)
import Data.ByteString.Lazy.Char8 as BS
import Data.List
import GHC.Int as GI
import Data.List.Split




findSubstring :: Eq a => [a] -> [a] -> Maybe Int
findSubstring pat str = Data.List.findIndex (Data.List.isPrefixOf pat) (Data.List.tails str) 

{-
fetchLinks :: String -> (String,String) -> [String]
fetchLinks [] _ = []
fetchLinks (x:xs)
-} 
{-使用了Data.List的函数-}
fetchTotal1 :: String -> String -> String -> [String]
fetchTotal1 str parstr1 parstr2 = Prelude.filter emptyFilter $ Prelude.map Prelude.head tails
                                where heads = splitOn parstr1 str
                                      tails = (Prelude.map (\x-> splitOn parstr2 x) heads)
                                      emptyFilter x = Prelude.length x > 0
bookUrl :: String
bookUrl  = "http://www.kanshu.com/files/article/html/111220/"


main :: IO ()
main = do
    content <- Prelude.readFile "./title.html"
    let links = fetchTotal1 content "<li>" "</li>"      
    let t= Prelude.map (BS.pack) links    
    mapM BS.putStrLn t    
    return ()


{-自己手写
fetchByHead :: String -> String -> Int -> [Maybe Int]
fetchByHead  [] _ _ = []
fetchByHead  str parstr startIndex = case findSubstring  parstr str of
                                      Just strIndex -> [Just (strIndex+startIndex)] ++ fetchByHead (Prelude.drop (strIndex + lengthStrPar) str) parstr (strIndex+startIndex)
                                      Nothing -> []
                                     where  lengthStrPar = Data.List.length parstr
                                
     
fetchTotal :: String -> String -> String -> [String]
fetchTotal  [] _ _ = []
fetchTotal str headStr tailStr = case  fetchHead str headStr of
                                    [] -> []
                                    strA ->  case fetchTail strA tailStr of
                                                 (h,t) ->  h : fetchTotal t headStr tailStr                                    


fetchHead ::String -> String -> String
fetchHead [] _ = []
fetchHead str parstr = case findSubstring parstr str of
                          Nothing -> []
                          Just strIndex -> Prelude.drop (strIndex + (Data.List.length parstr)) str


fetchTail :: String -> String->(String,String)
fetchTail [] _ = ([],[])
fetchTail str parstr = case findSubstring parstr str of
                                Nothing -> ([],[])
                                Just strIndex -> (Prelude.take strIndex str,Prelude.drop (strIndex + (Data.List.length parstr)) str)
 
-}

                    
{-

            


result <- get "http://www.kanshu.com/files/article/html/111220/"
result^.responseStatus
result^.responseStatus.statusCode
BS.putStrLn $ result^.responseBody

-}
             