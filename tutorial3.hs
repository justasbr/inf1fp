-- Informatics 1 - Functional Programming 
-- Tutorial 3
--
-- Week 5 - Due: 16/17 Oct.

import Data.Char
import Test.QuickCheck
import Data.List



-- 1. Map
-- a.
uppers :: String -> String
uppers str = map toUpper str

-- b.
doubles :: [Int] -> [Int]
doubles xs = map (*2) xs

-- c.        
penceToPounds :: [Int] -> [Float]
penceToPounds xs = map (\x -> fromIntegral x / 100) xs

-- d.
uppers' :: String -> String
uppers' xs = [toUpper x | x <- xs]

prop_uppers :: String -> Bool
prop_uppers str = (uppers str == uppers' str)



-- 2. Filter
-- a.
alphas :: String -> String
alphas xs = filter isAlpha xs

-- b.
rmChar ::  Char -> String -> String
rmChar c str = filter (/= c) str

-- c.
above :: Int -> [Int] -> [Int]
above num xs = filter (>num) xs

-- d.
unequals :: [(Int,Int)] -> [(Int,Int)]
unequals pairs = filter (\(a,b) -> a/=b) pairs

-- e.
rmCharComp :: Char -> String -> String
rmCharComp c xs = [x | x <- xs, x/=c]

prop_rmChar :: Char -> String -> Bool
prop_rmChar c str = (rmChar c str == rmCharComp c str)



-- 3. Comprehensions vs. map & filter
-- a.
upperChars :: String -> String
upperChars s = [toUpper c | c <- s, isAlpha c]

upperChars' :: String -> String
upperChars' s = map toUpper (filter isAlpha s)

prop_upperChars :: String -> Bool
prop_upperChars s = upperChars s == upperChars' s

-- b.
largeDoubles :: [Int] -> [Int]
largeDoubles xs = [2 * x | x <- xs, x > 3]

largeDoubles' :: [Int] -> [Int]
largeDoubles' xs = map (*2) (filter (>3) xs)

prop_largeDoubles :: [Int] -> Bool
prop_largeDoubles xs = largeDoubles xs == largeDoubles' xs 

-- c.
reverseEven :: [String] -> [String]
reverseEven strs = [reverse s | s <- strs, even (length s)]

reverseEven' :: [String] -> [String]
reverseEven' strs = map reverse (filter evenLen strs)
	where evenLen x = even $ length x

prop_reverseEven :: [String] -> Bool
prop_reverseEven strs = reverseEven strs == reverseEven' strs


-- 4. Foldr
-- a.
productRec :: [Int] -> Int
productRec []     = 1
productRec (x:xs) = x * productRec xs

productFold :: [Int] -> Int
productFold xs = foldr (*) 1 xs

prop_product :: [Int] -> Bool
prop_product xs = productRec xs == productFold xs

-- b.
andRec :: [Bool] -> Bool
andRec [] = True
andRec (x:xs) = x && andRec xs

andFold :: [Bool] -> Bool
andFold bools = foldr (&&) True bools

prop_and :: [Bool] -> Bool
prop_and xs = andRec xs == andFold xs 

-- c.
concatRec :: [[a]] -> [a]
concatRec [] = []
concatRec (x:xs) = x ++ concatRec xs

concatFold :: [[a]] -> [a]
concatFold xs = foldr (++) [] xs

prop_concat :: [String] -> Bool
prop_concat strs = concatRec strs == concatFold strs

-- d.
rmCharsRec :: String -> String -> String
rmCharsRec _ [] = []
rmCharsRec [] str = str
rmCharsRec (x:xs) str = rmCharsRec xs $ rmChar x str

rmCharsFold :: String -> String -> String
rmCharsFold chars str = foldr (rmChar) str chars

prop_rmChars :: String -> String -> Bool
prop_rmChars chars str = rmCharsRec chars str == rmCharsFold chars str



type Matrix = [[Int]]


-- 5
-- a.
uniform :: [Int] -> Bool
uniform ints = all (==(head ints)) ints

-- b.
valid :: Matrix -> Bool
valid matrix = allSameLen && length (head matrix) > 0
	where allSameLen = uniform lenList
		where lenList = map length matrix
	
-- 6.
-- uncurry :: (a->b->c) -> (a,b) -> c
zipWith_ :: (a-> b-> c) -> [a] -> [b] -> [c]
zipWith_ f xs ys = [f x y | (x,y) <- zip xs ys]

zipWith__ :: (a->b->c) -> [a] -> [b] -> [c]
zipWith__ f xs ys = (map . uncurry) f $ zip xs ys
-- 7.
plusM :: Matrix -> Matrix -> Matrix --TODO erros on invalid inputs
--plusM m1 m2 = [plusRow r1 r2| (r1,r2) <- zip m1 m2]
plusM m1 m2 | (length m1 == length m2) && (valid m1) && (valid m2) && ((length $ head m1) == (length $ head m2))
			--checking if m1 and m2 are valid and if they have the same # of rows and columns
			= (map. uncurry) plusRow $ zip m1 m2 
			| True = error("Bad input")
		  
plusRow :: [Int] -> [Int] -> [Int]
plusRow r1 r2 = zipWith (+) r1 r2

-- 8.

dotRow :: [Int] -> [Int] -> Int
dotRow r1 r2 = sum $ zipWith (*) r1 r2

timesM :: Matrix -> Matrix -> Matrix
timesM m1 m2 | (valid m1) && (valid m2) && ((length $ head m1) == (length m2)) 
			--Checking number of cols in m1 and number of rows in r2
			 = [[ dotRow row1 row2 | row2 <- (transpose m2)] | row1 <- m1]
			 | True = error("Bad input")

-- Optional material
-- 9.