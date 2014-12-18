import Data.List
import Data.Char
import System.Environment (getArgs)

--main = do
--	putStr $ show g

main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	putStr $ unlines $ map s1 $ lines input

g :: [String] -> String
g [] = []
g ((x:y):xs) | isUpper x = (x:y) ++ g xs
		 | True = g xs
--s1 :: String -> String
--s1 str = head str : [y | (x,y) <- zip str (tail str), x /= y]
--s1 str = join $ map capitalize $ words str
--	where
  --  capitalize (x:xs) = toUpper x : xs
   -- join (x:[]) = x
    --join (x:xs) = x ++ " " ++ join xs

--f :: String -> String
--f (x:[]) = [toUpper x]
--f (x:xs) | isAlpha x = toUpper x : f2 xs
--         | True = x : f xs
--	where
--	f2 (x:[]) = [toLower x]
--	f2 (x:xs) | isAlpha x = toLower x : f xs
--
--			  | True = x : f2 xs
--g :: Int
--g = sum $ take 1000 [x | x <- [2..], isPrime x]
--	where isPrime n = all (/=0) [mod n x | x <- [2..n-1]]
f :: String -> String
f x = show (fi n)
	where n = read x :: Integer

fi :: Integer -> Integer
fi n | n > 1 = fi (n-1) + fi (n-2)
	 | True = n

s :: String
s = "Shellless mollusk lives in wallless house in wellness. Aaaarrghh!!"

s1 :: String -> String
--s1 str = head str : [y | (x,y) <- zip str (tail str), x /= y]
s1 str = join $ map capitalize $ words str
	where
    capitalize (x:xs) = toUpper x : xs
    join (x:[]) = x
    join (x:xs) = x ++ " " ++ join xs