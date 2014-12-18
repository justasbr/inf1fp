-- Informatics 1 - Functional Programming 
-- Tutorial 2
--
-- Week 4 - due: 9/10 Oct.

import Data.Char
import Data.List
import Test.QuickCheck


-- 1.
rotate :: Int -> [Char] -> [Char]
rotate 0 x = x
rotate _ [] = []
rotate n (x:xs) | n > (length xs) = error "too large"
				| n < 0 = error "x negative"
				| True = rotate (n-1) (xs ++ [x])

-- 2.
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

-- 3. 
makeKey :: Int -> [(Char, Char)]
makeKey n = zip ['A'..'Z'] (rotate n ['A'..'Z'])

-- 4.
lookUp :: Char -> [(Char, Char)] -> Char
lookUp c xs = if not (null list) then head list else c
		where list = [b | (a,b) <- xs, a == c]

lookUpRec :: Char -> [(Char,Char)] -> Char
lookUpRec c [] = c
lookUpRec c (x:xs) | (fst x) == c = snd x
				   | otherwise = lookUp c xs


-- 5.
encipher :: Int -> Char -> Char
encipher n c = lookUp c (makeKey n) 

-- 6.
normalize :: String -> String
normalize xs = [toUpper x | x <- xs, isAlpha x || isDigit x]

-- 7.
encipherStr :: Int -> String -> String
encipherStr n xs = [encipher n x | x <- (normalize xs)]

-- 8.
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey xs = [(b,a) | (a,b) <- xs]

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec [] = []
reverseKeyRec ((a,b):xs) = (b,a) : reverseKeyRec xs

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey xs = (reverseKey xs == reverseKeyRec xs)
-- 9.
decipher :: Int -> Char -> Char
decipher n c = lookUp c (reverseKey (makeKey n))

decipherStr :: Int -> String -> String
decipherStr n xs = [decipher n x | x <- xs]

-- 10.
contains :: String -> String -> Bool
contains [] _ = False
contains str goal | isPrefixOf goal str = True
			      | otherwise = contains (tail str) goal

-- 11.
candidates :: String -> [(Int, String)]
candidates x = [ (n, decipherStr n x) | n <- [0..25],
				(decipherStr n x `contains` "AND" || decipherStr n x `contains` "THE")]



-- Optional Material

-- 12.
splitEachFive :: String -> [String]
splitEachFive [] = []
splitEachFive x | length x > 4 = (take 5 x) : (splitEachFive (drop 5 x))
				| otherwise = (take n x ++ replicate (5-n) 'X') : splitEachFive []
				where n = length x
splitEach :: Int -> String -> [String]
splitEach n [] = []
splitEach n x = (take n x) : (splitEach n (drop n x))

-- 13.
prop_transpose :: String -> Bool
prop_transpose x = (transpose (transpose (splitEachFive x)) == splitEachFive x)

-- 14.
makeStr :: [[Char]] -> String
makeStr [] = []
makeStr (x:xs) = x ++ makeStr xs

encrypt :: Int -> String -> String
encrypt n x = makeStr (transpose (splitEachFive (encipherStr n x)))

-- 15.
decrypt :: Int -> String -> String
decrypt n x = decipherStr n (makeStr (transpose (splitEach fifth x)))
			where fifth = div (length x) 5

prop_encrypt :: Int -> String -> Bool
prop_encrypt n x = ((normalize x) `isPrefixOf` (decrypt nSmall (encrypt nSmall x)))
			where nSmall = mod n 25
-- Challenge (Optional)

-- 16.
countFreqs :: String -> [(Char, Int)]
countFreqs str = [(x,c) | x<-['A'..'z'], let c = (length.filter (==x)) str, c>0]

lookUpInt :: Char -> [(Char, Int)] -> Int
lookUpInt c xs = if not (null list) then head list else 0
		where list = [b | (a,b) <- xs, a == c]

-- 17
freqDecipher :: String -> [String]
freqDecipher x = sortByFreq ([ decrypt n x | n <- [0..25]])

sortByFreq :: [String] -> [String]
sortByFreq [] = []
sortByFreq (x:xs) = sortByFreq([a | a <- xs, (freq 'E' a) > (freq 'E' x)]) ++ 
			  [x] ++ 
		      sortByFreq([b | b <- xs, (freq 'E' b) <= (freq 'E' x)])
			  where freq c str = lookUpInt c (countFreqs str)
			