import Data.List
import System.Environment (getArgs)

main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	putStr $ show g
	
	--unlines $ map f $ lines input

f :: String -> String
f str = map toNums $ split str
	where
	toNums "zero" = '0'
	toNums "one" = '1'
	toNums "two" = '2'
	toNums "three" = '3'
	toNums "four" = '4'
	toNums "five" = '5'
	toNums "six" = '6'
	toNums "seven" = '7'
	toNums "eight" = '8'
	toNums "nine" = '9'

split :: String -> [String]
split [] = []
split x = (firstWord) : (split rest)
	where
	firstWord = takeWhile (/=';') x
	rest = drop ((length firstWord)+1) x