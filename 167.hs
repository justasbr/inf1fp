import Data.Char
import System.Environment (getArgs)

main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	putStr $ unlines $ map f $ lines input
	
f :: String -> String
f str | (length str <= 55) = str
	  | elem ' ' (take 40 str) = trim (take 40 str) ++ "... <Read More>"
	  | True = (take 40 str) ++ "... <Read More>"
	where
	trim str | ((reverse str!!0) /= ' ') = trim (reverse (drop 1 $ reverse str))
		     | True = (reverse (drop 1 $ reverse str))