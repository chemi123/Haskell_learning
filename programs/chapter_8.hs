import Data.Char
import Control.Monad

main :: IO ()
main = doSample


doSample :: IO ()
doSample = do
  putStrLn "Hello, what's your name?"
  name <- getLine
  putStrLn $ "Hey " ++ name ++ ", you rock!"


fooSample :: IO ()
fooSample = do
  foo <- putStrLn "Hello, what's your name?"
  name <- getLine
  putStrLn $ "Hey " ++ name ++ ", you rock!"


letSample :: IO ()
letSample = do
  putStrLn "Hello, what's your first name?"
  firstName <- getLine
  putStrLn "Hello, what's your last name?"
  lastName <- getLine
  let bigFirstName = map toUpper firstName
      bigLastName = map toUpper lastName
  putStrLn $ "Hey, " ++ bigFirstName ++ " "
                     ++ bigLastName ++ "!"


echoString :: IO ()
echoString = do
  line <- getLine
  if null line
    then return ()
    else do
      putStrLn line
      echoString

whenSample :: IO ()
whenSample = do
  input <- getLine
  when (input == "hoge") $ putStrLn input


sequenceSample :: IO ()
sequenceSample = do
  rs <- sequence [getLine, getLine, getLine]
  print rs


forMSample :: IO ()
forMSample = do
  a <- forM [1..5] $ \i -> do
    print i
    return i
  print a


actions :: [IO ()]
actions =
  [ putStrLn "1"
  , putStrLn "2"
  , putStrLn "3"
  ]


sequence' :: [IO ()] -> IO [()]
sequence' [] = return [()]
sequence' (x:xs) = do
  x
  sequence' xs


mapM_' :: (Monad m) => (a -> m b) -> [a] -> m ()
mapM_' f [] = return ()
mapM_' f (x:xs) = do
  f x
  mapM_' f xs
