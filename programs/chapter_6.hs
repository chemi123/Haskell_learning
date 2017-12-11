import Data.List

wordsNum :: String -> [(String, Int)]
wordsNum = map (\ws -> (head ws, length ws)) . group . sort . words
