import Data.Char
import Data.List
import Geometry

wordsNum :: String -> [(String, Int)]
wordsNum = map (\ws -> (head ws, length ws)) . group . sort . words


digitSum :: Int -> Int
digitSum = sum . map digitToInt . show


firstTo :: Int -> Maybe Int
firstTo n = find (\x -> digitSum x == n) [1..]
