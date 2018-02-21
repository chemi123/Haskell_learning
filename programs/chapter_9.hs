import Control.Monad
import Control.Exception
import System.IO
import System.Environment
import System.Random

main :: IO ()
main = commandLineArgsSample

fileInput :: IO ()
fileInput =
  forever $ do
    l <- getLine
    putStrLn l


getContentsSample :: IO ()
getContentsSample = do
  l <- getContents
  putStr l


openFileSample :: IO ()
openFileSample = do
  handle <- openFile "tmp" ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle


withFileSample :: IO ()
withFileSample =
  withFile "tmp" ReadMode $ \handle -> do 
    contents <- hGetContents handle
    putStr contents


withFile' :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFile' file mode f =
  bracket (openFile file mode)
          (\handle -> hClose handle)
          (\handle -> f handle)


readFileSample :: IO ()
readFileSample = do
  contents <- readFile "tmp"
  putStr contents


writeFileSample :: IO ()
writeFileSample = writeFile "tmp" "hoge\nfuga\npiyo"


appendFileSample :: IO ()
appendFileSample = appendFile "tmp" "hoge\nfuga\npiyo"


commandLineArgsSample :: IO ()
commandLineArgsSample = do
  args <- getArgs
  progName <- getProgName
  putStrLn "The arguments:"
  mapM_ putStrLn args
  putStrLn "The program name:"
  putStrLn progName
