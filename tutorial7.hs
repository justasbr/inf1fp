-- Informatics 1 - Functional Programming 
-- Tutorial 7
--
-- Week 9 - Due: 13/14 Nov.


import LSystem
import Test.QuickCheck

-- Exercise 1

-- 1a. split
split :: Command -> [Command]
split (Sit :#: xs) = split xs
split (x :#: xs) = split x ++ split xs
split Sit = []
split x = [x]

-- 1b. join
join' :: [Command] -> Command
--join' [] = Sit
join' (x:[]) = x
join' (x:xs) = (x :#: (join' xs))



-- 1c  equivalent
equivalent :: Command -> Command -> Bool
equivalent a b = split a == split b

-- 1d. testing join and split
prop_split_join :: Command -> Bool
prop_split_join c = equivalent c (join' (split c))

prop_split :: Command -> Bool
prop_split xs = and [goodCommand x | x <- (split xs)]
	where
	goodCommand (x :#: y) = False
	goodCommand Sit = False
	goodCommand _ = True

-- Exercise 2
-- 2a. copy
copy :: Int -> Command -> Command
copy n c = join' [c | x <- [1..n]]

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon d = copy 5 (Go d :#: Turn 72)

-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon d n = copy n (Go d :#: Turn (angle))
	where
	angle = 360.0 / (fromIntegral n)

prop_polygon_pentagon :: Float -> Bool
prop_polygon_pentagon d = equivalent (pentagon d) (polygon d 5)
-- Exercise 3
-- spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command
spiral _    0 _ _ = Sit
spiral side n step angle = join' [Go (side + ((fromIntegral x)*step)) :#: Turn angle | x <- [0..n-1], (side + ((fromIntegral x)*step)) >= 0]

-- Exercise 4
-- optimise
optimise :: Command -> Command
optimise c | split c == op (split c) = c
		   | otherwise = optimise $ join' ( op $ split c)
	where
	op [] = []
	op ((Sit):xs) = xs
	op ((Go a):(Go b):xs) = (Go (a+b)) : op xs
	op ((Go 0):xs) = op xs
	op ((Turn 0):xs) = op xs
	op (x:xs) = x : op xs
---optimise (Turn a :#: Turn b)= Turn (a+b)
--optimise (Turn 0) = Sit
--optimise (Go 0) = Sit
--optimise (x :#: Sit) = optimise x
--optimise (Sit :#: x) = optimise x
--optimise (x :#: y) = x 

-- L-Systems

-- 5. arrowhead
arrowhead :: Int -> Command
arrowhead = undefined

-- 6. snowflake
snowflake :: Int -> Command
snowflake = undefined

-- 7. hilbert
hilbert :: Int -> Command
hilbert = undefined

