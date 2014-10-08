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
lookUp c xs = head ([b | (a,b) <- xs, a == c] ++ [c])

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
splitEachFive = undefined

-- 13.
prop_transpose :: String -> Bool
prop_transpose = undefined

-- 14.
encrypt :: Int -> String -> String
encrypt = undefined

-- 15.
decrypt :: Int -> String -> String
decrypt = undefined

-- Challenge (Optional)

-- 16.
countFreqs :: String -> [(Char, Int)]
countFreqs = undefined

-- 17
freqDecipher :: String -> [String]
freqDecipher = undefined