-- Informatics 1 - Functional Programming 
-- Tutorial 4
--
-- Due: the tutorial of week 6 (23/24 Oct)

import Data.List (nub)
import Data.Char
import Test.QuickCheck
import Network.HTTP (simpleHTTP,getRequest,getResponseBody)

-- <type decls>

type Link = String
type Name = String
type Email = String
type HTML = String
type URL = String

-- </type decls>
-- <sample data>
testURL     = "http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/testpage.html"

testHTML :: String
testHTML =    "<html>"
           ++ "<head>"
           ++ "<title>FP: Tutorial 4</title>"
           ++ "</head>"
           ++ "<body>"
           ++ "<h1>A Boring test page</h1>"
           ++ "<h2>for tutorial 4</h2>"
           ++ "<a href=\"http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/\">FP Website</a><br>"
           ++ "<b>Lecturer:</b> <a href=\"mailto:dts@inf.ed.ac.uk\">Don Sannella</a><br>"
           ++ "<b>TA:</b> <a href=\"mailto:m.k.lehtinen@sms.ed.ac.uk\">Karoliina Lehtinen</a>"
           ++ "</body>"
           ++ "</html>"

testLinks :: [Link]
testLinks = [ "http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/\">FP Website</a><br><b>Lecturer:</b> "
            , "mailto:dts@inf.ed.ac.uk\">Don Sannella</a><br><b>TA:</b> "
            , "mailto:m.k.lehtinen@sms.ed.ac.uk\">Karoliina Lehtinen</a></body></html>" ]


testAddrBook :: [(Name,Email)]
testAddrBook = [ ("Don Sannella","dts@inf.ed.ac.uk")
               , ("Karoliina Lehtinen","m.k.lehtinen@sms.ed.ac.uk")]

-- </sample data>
-- <system interaction>

getURL :: String -> IO String
getURL url = simpleHTTP (getRequest url) >>= getResponseBody

emailsFromURL :: URL -> IO ()
emailsFromURL url =
  do html <- getURL url
     let emails = (emailsFromHTML html)
     putStr (ppAddrBook emails)

emailsByNameFromURL :: URL -> Name -> IO ()
emailsByNameFromURL url name =
  do html <- getURL url
     let emails = (emailsByNameFromHTML html name)
     putStr (ppAddrBook emails)

-- </system interaction>
-- <exercises>

-- 1.
sameString :: String -> String -> Bool
sameString a b = map toUpper a == map toUpper b


-- 2.
prefix :: String -> String -> Bool
prefix a b = sameString a (take (length a) b)

prop_prefix :: String -> Int -> Bool
prop_prefix str n  =  prefix substr (map toLower str) &&
		      prefix substr (map toUpper str)
                          where
                            substr  =  take n str

tails' :: String -> [String]
tails' [] = [""]
tails' x = x : tails' (tail x)

-- 3.
contains :: String -> String -> Bool
contains s b = foldr (||) False $ map (\x -> prefix s x) (tails' b)

prop_contains :: String -> Int -> Int -> Bool
prop_contains str x y = contains substr (map toUpper str) && 
						contains substr (map toLower str)
	where
	substr = drop y (take x str)

-- 4.
takeUntil :: String -> String -> String
takeUntil s b | contains s b = head [take (length b - y) b | (x,y) <- zip (map (prefix s) (tails' b)) ([length b,(length b) - 1..0]), x]
			  | True = b
			  
dropUntil :: String -> String -> String
dropUntil s b | contains s b = head [drop ((length b) - y + (length s)) b | (x,y) <- zip (map (prefix s) (tails' b)) ([length b,(length b) - 1..0]), x]
			  | True = b


-- 5.
split :: String -> String -> [String]
split [] _ = error("empty separator")
split a str | contains a str = (takeUntil a str) : (split a (dropUntil a str))
			| True = [str]

--connect :: String -> String -> String -> String
--connect a b sep :: a ++ sep ++ b

reconstruct :: String -> [String] -> String
reconstruct a [x] = x
reconstruct a (x:xs) = x ++ a ++ (reconstruct a xs)

prop_split :: Char -> String -> String -> Bool
prop_split c sep str = reconstruct sep' (split sep' str) `sameString` str
  where sep' = c : sep

-- 6.
linksFromHTML :: HTML -> [Link]
linksFromHTML src= split aTag (dropUntil aTag src)
	where aTag ="<a href=\""
	

testLinksFromHTML :: Bool
testLinksFromHTML  =  linksFromHTML testHTML == testLinks


-- 7.
takeEmails :: [Link] -> [Link]
takeEmails links = filter (prefix "mailto:") links


-- 8.
link2pair :: Link -> (Name,Email)
link2pair link = (last res, head res)
	where res = split "\">" (takeUntil "</a>" (dropUntil "mailto:" link) )

-- 9.
emailsFromHTML :: HTML -> [(Name,Email)]
emailsFromHTML src = map link2pair (takeEmails (linksFromHTML src))

testEmailsFromHTML :: Bool
testEmailsFromHTML  =  emailsFromHTML testHTML == testAddrBook


-- 10.
findEmail :: Name -> [(Name, Email)] -> [(Name, Email)]
findEmail name list = filter (\x -> contains name (fst x)) list


-- 11.
emailsByNameFromHTML :: HTML -> Name -> [(Name,Email)]
emailsByNameFromHTML src name = findEmail name (emailsFromHTML src)


-- Optional Material

-- 12.
ppAddrBook :: [(Name, Email)] -> String
ppAddrBook addr = unlines [ name ++ ": " ++ email | (name,email) <- addr ]