head' :: [a] -> a
head' [] = error "empty list!"
head' (x:_) = x

firstLetter :: String -> String
firstLetter "" = "Empty String!"
firstLetter all@(x:xs) = [x] ++ " is the first letter of " ++ all

bmiTell1 :: Double -> String
bmiTell1 bmi
  | bmi <= 18.5 = "too light!"
  | bmi <= 25.0 = "normal"
  | bmi <= 30.0 = "fat!"
  | otherwise   = "whale!"


bmiTell2 :: Double -> Double -> String
bmiTell2 w h
  | w / h^2 <= 18.5 = "too light!"
  | w / h^2 <= 25.0 =  "normal"
  | w / h^2 <= 30.0 =  "fat!"
  | otherwise       = "whale!"

bmiTell3 :: Double -> Double -> String
bmiTell3 w h
  | bmi <= 18.5 = "too light!"
  | bmi <= 25.0 = "normal"
  | bmi <= 30.0 = "fat!"
  | otherwise    = "whale!"
  where bmi = w / h^2

cylinder :: Double -> Double -> Double
cylinder r h = 
  let sideArea = 2 * pi * r * h
      topArea = pi * r^2
  in sideArea + 2 * topArea

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h^2]

describeList :: [a] -> String
describeList ls =
  "The list is "
  ++ case ls of [] -> "empty."
                [x] -> "a singleton list."
                _ -> "a longer list."
