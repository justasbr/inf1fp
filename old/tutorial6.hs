-- Informatics 1 Functional Programming
-- Tutorial 6
--
-- Due: 6/7 November

import System.Random
import Test.QuickCheck

-- Importing the keymap module

import KeymapTree


-- Type declarations

type Barcode = String
type Product = String
type Unit    = String

type Item    = (Product,Unit)

type Catalogue = Keymap Barcode Item


-- A little test catalog

testDB :: Catalogue
testDB = fromList [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]


-- Exercise 1

longestProductLen :: [(Barcode, Item)] -> Int
longestProductLen c = foldr max 0 [length (name item) | item <- c]
	where name (barcode,(product,unit)) = product

formatLine :: Int -> (Barcode, Item) -> String
formatLine i (barcode,(product,unit)) = foldl (joinWithDots) barcode [normalized product, unit]
	where 
	joinWithDots a b = a ++ "..." ++ b
	normalized product = product ++ replicate (i - length product) '.'

showCatalogue :: Catalogue -> String
showCatalogue c = foldr (newLineBetween) [] formattedList
	where 
	newLineBetween a b = a ++ "\n" ++ b
	formattedList = map (formatLine len) clist
	clist = toList c
	len = longestProductLen clist
     
-- Exercise 2
maybeToList :: Maybe a -> [a]
maybeToList a = case a of
					Nothing -> []
					Just x -> [x]

maybeToList2 :: Maybe a -> [a]
maybeToList2 Nothing = []
maybeToList2 (Just x) = [x]

prop_maybes :: Maybe Int -> Bool
prop_maybes x = maybeToList x == maybeToList2 x

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

catMaybes :: [Maybe a] -> [a]
catMaybes xs = [x | Just x <- xs]

-- Exercise 3

getItems :: [Barcode] -> Catalogue -> [Item]
getItems barcodes c = catMaybes [get barcode c | barcode <- barcodes]

-- Input-output ------------------------------------------

readDB :: IO Catalogue
readDB = do dbl <- readFile "database.csv"
            let db = fromList (map readLine $ lines dbl)
            putStrLn (size db >= 0 `seq` "Done")
            return db

readLine :: String -> (Barcode,Item)
readLine str = (a,(c,b))
    where
      (a,str2) = splitUpon ',' str
      (b,c)    = splitUpon ',' str2

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs) | x == c    = ("",xs)
                   | otherwise = (x:ys,zs)
                   where
                     (ys,zs) = splitUpon c xs

getSample :: Catalogue -> IO Barcode
getSample db = do g <- newStdGen
                  return $ fst $ toList db !! fst (randomR (0,size db - 1) g)