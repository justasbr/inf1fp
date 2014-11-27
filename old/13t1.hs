import Data.Char
import Test.QuickCheck

f :: String -> Int
f str = sum [(digitToInt x) * (3^power) | (x,power) <- zip (reverse str) [0..]]

g :: String -> Int
g str = g_helper (reverse str) 0
	where
	g_helper :: String -> Int -> Int
	g_helper [] _ = 0
	g_helper (x:xs) n = (digitToInt x * (3^n)) + g_helper xs (n+1)
	
test_fg :: Int -> Bool
test_fg int | int > 0 = f (show int) == g (show int)
			| otherwise = f (show (-int)) == g (show (-int))

p :: [Int] -> Bool
p [] = error "empty list"
p (x:xs) | x == 0    = error "first element is 0"
		 | otherwise = foldr (&&) True [n `mod` x == 0 | n <- (x:xs), n>0]

q :: [Int] -> Bool
q [] = error "empty list"
q xs | (head xs == 0) = error "first element is 0"
	 | otherwise = q_helper xs (head xs)
	where
	q_helper [] n = True
	q_helper (x:xs) n | x > 0 = ((x `mod` n) == 0) && q_helper xs n
					  | otherwise = q_helper xs n

r :: [Int] -> Bool
r [] = error "empty list"
r xs | (head xs) == 0 = error "first element is 0"
	 | otherwise = foldr (&&) True $ map (\x -> (x `mod` (head xs)) == 0) $ filter (>0) $ xs
	 
test_pqr :: [Int] -> Bool
test_pqr ints | (length ints > 0) && (head ints /= 0) = (p ints == q ints) && (p ints == r ints)
			  | otherwise = True

data Expr = X -- variable
		  | Const Int -- integer constant
		  | Neg Expr -- negation
		  | Expr :+: Expr -- addition
		  | Expr :*: Expr -- multiplication
		  
rpn :: Expr -> [String]
rpn (x :*: y) = rpn x ++ rpn y ++ ["*"]
rpn (x :+: y) = rpn x ++ rpn y ++ ["+"]
rpn (Neg x)   = rpn x ++ ["-"]
rpn (Const n) = [show n]
rpn (X)       = ["X"]

evalrpn :: [String] -> Int -> Int
evalrpn xs val = evalrpn_helper xs val []
	where
	evalrpn_helper [] _ list = head list
	evalrpn_helper ("X":xs) val list = evalrpn_helper xs val (val:list)
	evalrpn_helper ("-":xs) val list = evalrpn_helper xs val (-(head list):(tail list))
	evalrpn_helper ("*":xs) val list = evalrpn_helper xs val ((list!!0 * list!!1):(drop 2 list))
	evalrpn_helper ("+":xs) val list = evalrpn_helper xs val ((list!!0 + list!!1):(drop 2 list))
	evalrpn_helper (n:xs) val list = evalrpn_helper xs val ((read n):list)
	
