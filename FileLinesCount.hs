module FileLinesCount where
 
import System.Directory
import System.IO
import Control.Applicative
  
main :: IO ()
main = do
        total <- countDir "./"
        print $ total
        return ()
        
data NameWithLength=NameWithLength String Int deriving Show


countDir :: FilePath -> IO [NameWithLength]
countDir fp =  do
             f <- getDirectoryContents "."
             all<-sequence [countFile ff |  ff <- f, elem ff ["..","."] == False]
             return $ concat all

countFile :: FilePath -> IO [NameWithLength]
countFile  a  = do
              flag <- doesDirectoryExist a
              case flag of
                 True -> countDir a
                 False -> sequence [getLineByFile a]
                 
getLineByFile :: FilePath -> IO NameWithLength
getLineByFile fp = do
                contents <- readFile fp
                let all = lines contents
                return $ NameWithLength fp (length all)