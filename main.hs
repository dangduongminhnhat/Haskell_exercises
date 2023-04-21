fibonancci :: Int -> Int
fibonancci 0 = 1
fibonancci 1 = 1
fibonancci n = fibonancci (n - 1) + fibonancci (n - 2)


findMaxElement :: [Int] -> Int
findMaxElement a = if (length a) == 1 then head a 
                else if x > y then x else y
                    where {x = head a; y = findMaxElement (tail a)}

sumEven :: [Int] -> Int
sumEven [] = 0
sumEven a = if mod x 2 == 0 then x + y
            else y
                where {x = head a; y = sumEven (tail a)}

-- Trả về số lần xuất hiện của biến a trong list
count :: Int -> [Int] -> Int
count _ [] = 0
count x a = if head a == x then 1 + count x (tail a)
            else count x (tail a)


-- Tìm vị trí của a trong mảng
find :: Int -> [Int] -> Int
find _ [] = -1
find x xs = if head xs == x then 0
            else if y == -1 then -1
                else 1 + y
                where y = find x (tail xs) 

-- groupRuns function

-- Trả về chuỗi dài nhất trong mãng
findTheLongestString :: [String] -> String
findTheLongestString [] = ""
findTheLongestString xs = if (length (head xs)) > (length y) then head xs
                                else y
                                where y = findTheLongestString (tail xs)

-- Trả về mảng bình phương
squareArray :: [Int] -> [Int]
squareArray [] = []
squareArray xs = [x * x | x <- xs]

-- Trả về chuỗi đảo ngược
reverseString :: String -> String
reverseString "" = ""
reverseString s = reverseString (tail s) ++ [x | (x, i) <- zip s [0..], i == 0] 

-- Trả về mảng trị tuyệt đối
absoluteNumber :: Int -> Int
absoluteNumber x = if x >= 0 then x else (0 - x)

absoluteArray :: [Int] -> [Int]
absoluteArray [] = []
absoluteArray xs = map absoluteNumber xs

-- Trả về các phần tử trùng nhau của hai mảng
eraseSameElements :: [Int] -> [Int]
eraseSameElements [] = []
eraseSameElements xs = foldl (\ys x -> if (find x ys) == -1 then (ys ++ [x]) else ys) [] xs

sameElements :: [Int] -> [Int] -> [Int]
sameElements xs ys = eraseSameElements [a | a <- xs, b <- ys, a == b]

-- Trả về tích các phần tử trong mảng
productElements :: [Int] -> Int
productElements [] = 0
productElements [x] = x
productElements (x:xs) = x * productElements xs

-- Trả về các phần tử chẵn của mảng
evenList :: [Int] -> [Int]
evenList xs = [x | x <- xs, x `mod` 2 == 0]

-- Trả về chuỗi con tăng dài nhất torng mảng
longestIncreaseSubsequence :: [Int] -> [Int]

main:: IO()
main = print (evenList [1,2,3])

