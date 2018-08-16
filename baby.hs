lucky :: Int -> String

lucky 7 = "LUCKEY NUMBER SEVEN"
lucky x = "Sorry, you're out of luck, pal!"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Double ,Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a,b, c) -> c
third (_, _, z) = z

firstLetter :: String -> String
firstLetter "" = "Empty string, whoops"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: Double -> Double -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat = "You're a whale, congulations!"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat = 30.0

head' :: [a] -> a
head' [] = error "No head for empty lists"
head' (x:_) = x

head' :: [a] -> a
head' xs = case xs of [] -> error "No head for empty list"
                      (x:_) -> x

describeList :: [a] -> String
describeList ls = "The list is " ++ case ls of [] -> "empty"
    [x] -> "a singleton list"
    xs -> "a longer list."




