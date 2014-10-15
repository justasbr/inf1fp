import Data.Char
import Data.List

double x = x + x
double2 x = double x + double x

squares :: [Integer] -> [Integer]
squares xs = [x*x | x <- xs]

square :: Integer -> Integer
square x = x*x

squaresRec :: [Integer] -> [Integer]
squaresRec [] = []
squaresRec (x:xs) = x*x : squaresRec xs

powerOf4 :: [Integer] -> [Integer]
powerOf4 xs = [square x | x <- squares xs]

fac :: Integer -> Integer
fac 0 = 1
fac x = x * fac (x-1)


halveEvensRec :: [Int] -> [Int]
halveEvensRec [] = []
halveEvensRec (x:xs) | (even x)  = [div x 2] ++ halveEvensRec xs
                     | otherwise = halveEvensRec xs
					 
inRangeRec :: Int ->  Int -> [Int] -> [Int]
inRangeRec lo hi [] = []
inRangeRec lo hi (x:xs) | ((x>=lo) && (x<=hi)) = [x]++ inRangeRec lo hi xs
						| otherwise			   = inRangeRec lo hi xs

-- Helper function
discount :: Int -> Int
discount price = round (fromIntegral price * 0.9)

-- List-comprehension version of pennypincher
pennypincher :: [Int] -> Int
--pennypicher prices = [discount price | price <- prices]
pennypincher prices = sum[ discount x | x <- prices, (discount x <= 19900)]

-- Use capitalise from the previous question.
capitaliseRec :: String -> String
capitaliseRec [] = []
capitaliseRec (x:xs) = toUpper x : lowerRec xs

lowerRec :: String -> String
lowerRec [] = []
lowerRec (x:xs) = toLower x : lowerRec xs
-- Recursive version
--titleRec :: [String] -> [String]
-- titleRec [] = []
-- -titleRec (w:words) = [capitaliseRec w] ++ normalise words

-- normalise :: [String] -> [String] -> [Int]
-- normalise [] = []
-- normalise (w:words) | (length w > 3) = [capitaliseRec w] ++ normalise words
					-- | otherwise = [lowerRec w] ++ normalise words

--searchRec :: String -> Char -> [Int]
--searchRec [] _ = []
--searchRec (x:xs) goal
--	| ( x == goal) = [n+1 | -1 : n <- occurances]
--    | otherwise = [n+1 | n <- occurances]
--    where occurances = searchRec xs goal

	
containsRec :: String -> String -> Bool
--containsRec [] _ = False
containsRec str goal | isPrefixOf goal str = True
					 | otherwise = containsRec (tail str) goal
					 
fromTo :: Int -> Int -> [Int]
fromTo m n | m > n = []
		   | otherwise = m : fromTo (m+1) n

fromToOdds :: Int -> Int -> [Int]
fromToOdds m n = [x | x <- fromTo m n, odd x]

factorial :: Int -> Int
factorial m | m <= 0 = 1
			| otherwise = m * factorial (m-1)
			

lookUp :: Char -> [(Char,Char)] -> Char
lookUp c [] = c
lookUp c (x:xs) | (fst x) == c = snd x
				| otherwise = lookUp c xs
				
chain :: (Integral a) => a -> [a]  
chain 1 = [1]  
chain n  
    | even n =  n:chain (n `div` 2)  
    | odd n  =  n:chain (n*3 + 1)  
				
numLongChains :: Int  
numLongChains = length (filter isLong (map chain [1..100]))  
    where isLong xs = length xs > 15 

map' :: (a -> b) -> [a] -> [b]  
map' f xs = foldr (\x acc -> f x : acc) [] xs

map' f xs = foldl (\acc x -> acc ++ [f x]) [] xs
