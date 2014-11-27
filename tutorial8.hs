-- Informatics 1 - Functional Programming 
-- Tutorial 8
--
-- Week 11 - due: 28/29 Nov.

import Data.List
import Test.QuickCheck



-- Type declarations

type FSM q = ([q], Alphabet, q, [q], [Transition q])
type Alphabet = [Char]
type Transition q = (q, Char, q)



-- Example machines

m1 :: FSM Int
m1 = ([0,1,2,3,4],
      ['a','b'],
      0,
      [4],
      [(0,'a',1), (0,'b',1), (0,'a',2), (0,'b',2),
       (1,'b',4), (2,'a',3), (2,'b',3), (3,'b',4),
       (4,'a',4), (4,'b',4)])

m2 :: FSM Char
m2 = (['A','B','C','D'],
      ['0','1'],
      'B',
      ['A','B','C'],
      [('A', '0', 'D'), ('A', '1', 'B'),
       ('B', '0', 'A'), ('B', '1', 'C'),
       ('C', '0', 'B'), ('C', '1', 'D'),
       ('D', '0', 'D'), ('D', '1', 'D')])

dm1 :: FSM [Int] 
dm1 =  ([[],[0],[1,2],[3],[3,4],[4]],
        ['a','b'],
        [0],
        [[3,4],[4]],
        [([],   'a',[]),
         ([],   'b',[]),
         ([0],  'a',[1,2]),
         ([0],  'b',[1,2]),
         ([1,2],'a',[3]),
         ([1,2],'b',[3,4]),
         ([3],  'a',[]),
         ([3],  'b',[4]),
         ([3,4],'a',[4]),
         ([3,4],'b',[4]),
         ([4],  'a',[4]),
         ([4],  'b',[4])])



-- 1.
states :: FSM q -> [q]
states (u,a,s,f,t) = u

alph   :: FSM q -> Alphabet
alph (u,a,s,f,t) = a

start  :: FSM q -> q
start (u,a,s,f,t) = s

final  :: FSM q -> [q]
final (u,a,s,f,t) = f

trans  :: FSM q -> [Transition q]
trans (u,a,s,f,t) = t


-- 2.
delta :: (Eq q) => FSM q -> q -> Char -> [q]
delta fsm state input = [f | (s,i,f) <- (trans fsm), s == state && input == i]


-- 3.
accepts :: (Eq q) => FSM q -> String -> Bool
accepts m xs = acceptsFrom m (start m) xs

acceptsFrom :: (Eq q) => FSM q -> q -> String -> Bool
acceptsFrom m q [] = q `elem` final m
acceptsFrom m q (x:xs) = or [ acceptsFrom m state xs | state <- delta m q x]

-- 4.
canonical :: (Ord q) => [q] -> [q]
canonical xs = sort $ nub xs


-- 5.
ddelta :: (Ord q) => FSM q -> [q] -> Char -> [q]
ddelta fsm superstate input = canonical $ concat [delta fsm state input | state <- superstate]


-- 6.
next :: (Ord q) => FSM q -> [[q]] -> [[q]]
next fsm states = canonical $ states ++ [ddelta fsm state input | state <- states, input <- (alph fsm)]


-- 7.
reachable :: (Ord q) => FSM q -> [[q]] -> [[q]]
reachable fsm states | states == (next fsm states) = states
					 | True = reachable fsm (next fsm states)


-- 8.
dfinal :: (Ord q) => FSM q -> [[q]] -> [[q]]
dfinal fsm superstates = [superstate | superstate <- superstates, is_final superstate]
	where
	is_final superstate = or [state `elem` (final fsm) | state <- superstate]


-- 9.
dtrans :: (Ord q) => FSM q -> [[q]] -> [Transition [q]]
dtrans fsm states = [(state1,symbol,(ddelta fsm state1 symbol)) | state1 <- states, symbol <- alph fsm]


-- 10.
deterministic :: (Ord q) => FSM q -> FSM [q]
deterministic fsm = (u,a,s,f,t)
	where
	s = [start fsm]
	u = reachable fsm [s] --reachable states
	a = alph fsm
	f = dfinal fsm u --final states
	t = dtrans fsm u


