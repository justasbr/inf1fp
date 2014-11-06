import Data.Char
import Data.List
import Test.QuickCheck
import qualified Data.Map as Map

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

d :: [Int] -> [Int]
d [] = []
d [x] = []
d (x:y:xs) | x == y = x : d (y:xs)
		   | True = d (y:xs)
		 
--c :: [Int] -> [Int]
--c xs = [a | (a,b) <- zip xs (tail xs), a==b]

--prop_cd :: [Int] -> Bool
--prop_cd xs = c xs == d xs
	
f :: Char -> Int
f c | isDigit c = ord c - ord '0'
	| elem (toUpper c) ['A'..'F'] = ord (toUpper c) - ord 'A' + 10
	| True = error("bad")
	
isHex :: Char -> Bool
isHex c = (isDigit c || elem (toUpper c) ['A'..'F'])
	
g :: String -> Int
g xs = maximum (-1 : [f x | x <- xs, isHex x])

h :: String -> Int
h [] = -1
h (x:xs) | isHex x = max (f x) (h xs)
		 | True = h xs

c :: [Int] -> Int
c [] = error("empty list")
c xs = product [a-b | (a,b) <- zip xs (tail xs)]

dd :: [Int] -> Int
dd [] = error("empty list")
dd [x] = 1
dd (x:y:xs) = (x-y) * dd (y:xs)

prop_cdd :: [Int] -> Bool
prop_cdd xs = null xs || c xs == dd xs

aa :: Char -> Bool
aa c | elem (toUpper c) ['A'..'M'] = True
	 | elem (toUpper c) ['N'..'Z'] = False
	 | True = error("not alphabet letter")
bb :: String -> Bool
bb xs = firstHalf > secondHalf
	where 
	firstHalf = length [x | x<-xs, isAlpha x, aa x]
	secondHalf = length [x | x<-xs, isAlpha x, not (aa x)]

rbb :: String -> Bool
rbb str = score str > 0
	where
	score [] = 0
	score (x:xs) | not (isAlpha x) = score xs
				 | aa x = 1 + score xs
				 | not (aa x) = (-1) + score xs
prop_bbrbb :: String -> Bool
prop_bbrbb str = bb str == rbb str

f2013 :: Char -> Int
f2013 c | elem c "HASKELL" = 10
		| elem c ['A'..'Z'] = 2
		| elem c "haskell" = 5
		| elem c ['a'..'z'] = 1
		| True = 0
g2013 :: String -> Int
g2013 str = product [f2013 x | x <- str, isAlpha x]

g2013r :: String -> Int
g2013r [] = 1
g2013r (x:xs) | isAlpha x = (f2013 x) * (g2013r xs)
			  | True = g2013r xs
prop_g2013 :: String -> Bool
prop_g2013 str = g2013 str == g2013r str

c2013 :: String -> String -> String
c2013 a b = [x | (x,y) <- zip a b, x==y]

c2013r :: String -> String -> String
c2013r [] _ = []
c2013r _ [] = []
c2013r (x:xs) (y:ys) | x==y = x : c2013r xs ys
					 | True = c2013r xs ys
					 
prop_c2013 :: String -> String -> Bool
prop_c2013 x y = c2013 x y == c2013r x y

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

sameString :: String -> String -> Bool
sameString a b = map toUpper a == map toUpper b
m = Map.empty

data Exp = Lit Int
		 | Add Exp Exp
		 | Mul Exp Exp
evalExp :: Exp -> Int
evalExp (Lit n) = n
evalExp (Add e f) = evalExp e + evalExp f
evalExp (Mul e f) = evalExp e * evalExp f

showExp :: Exp -> String
showExp (Lit n) = show n
showExp (Add e f) = par (showExp e ++ "+" ++ showExp f)
showExp (Mul e f) = par (showExp e ++ "*" ++ showExp f)

par :: String -> String
par s = "(" ++ s ++ ")"

strToInt :: String -> Int
strToInt str = read str :: Int