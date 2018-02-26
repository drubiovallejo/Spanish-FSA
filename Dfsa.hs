-- DFSA 1.0
-- Jeff Heinz
-- 29 Mar 2017

-- This file provides a Module for implementing Deterministic
-- Finite-State String Acceptors (DFSA).

-- We import the Data.List module because it provides functions "nub"
--  which removes duplicates in a list and "intersect" which only
--  keeps elements common to two lists. 
--
-- The documentation is here. 
-- https://hackage.haskell.org/package/base-4.9.1.0/docs/Data-List.html

module Dfsa
( DFSA(A) -- this exports the data type DFSA ands its constructor A
, states
, sigma
, start
, finals
, delta
, recognize
, intersection
, union
, complement
, difference
, subset
, isEmpty
) where


import Data.List as List (nub,intersect)  -- this ONLY imports the
                                          -- functions nub and
                                          -- intersect from Data.List

 -- here we introduce a data structure for DFSA 
data DFSA q b = A [q] [b] q [q] (q -> b -> q)

-- These functions take a DFSA A as input and output the states, the
-- alphabet, the start state, the final states, and the delta function
-- of A, respectively. 

states :: DFSA q b -> [q]
states (A qs _ _ _ _) = qs 

sigma :: DFSA q b -> [b]
sigma (A _ bs _ _ _) = bs 

start :: DFSA q b -> q
start (A _ _ q0 _ _) = q0

finals :: DFSA q b -> [q]
finals (A _ _ _ fs _) = fs

delta :: DFSA q b -> q -> b -> q
delta (A _ _ _ _ d) = d

dstar = foldl

-- The function 'recognize' takes a dfsa A as its first argument, a
-- string w as its second argument, and returns TRUE only if A
-- recognizes w and FALSE otherwise. Its type signature is given
-- below.

recognize :: Eq q => DFSA q b -> [b] -> Bool 
recognize a w = elem q (finals a) where q = dstar (delta a) (start a) w

-- Note these functions assume the DFSA are complete.

intersection :: (Eq q, Eq r, Eq b) => DFSA q b -> DFSA r b -> DFSA (q,r) b
intersection (A qs1 bs1 q01 fs1 d1) (A qs2 bs2 q02 fs2 d2) =
             (A qs bs q0 fs d) 
             where
               qs = [(x,y) | x <- qs1, y <- qs2 ]
               bs = List.nub (bs1 ++ bs2)
               q0 = (q01,q02)
               fs = filter (\(x,y) -> elem x fs1 && elem y fs2) qs
--             fs = [(x,y) | x <- fs1, y <- fs2 ]
               d (x,y) a = (d1 x a, d2 y a)

union :: (Eq q, Eq r, Eq b) => DFSA q b -> DFSA r b -> DFSA (q,r) b
union (A qs1 bs1 q01 fs1 d1) (A qs2 bs2 q02 fs2 d2) =
      (A qs bs q0 fs d) 
      where
        qs = [(x,y) | x <- qs1, y <- qs2 ]
        bs = List.nub (bs1 ++ bs2)
        q0 = (q01,q02)
        fs = filter (\(x,y) -> elem x fs1 || elem y fs2) qs
        d (x,y) a = (d1 x a, d2 y a)


complement :: Eq q => Eq b => DFSA q b -> DFSA q b
complement (A qs bs q0 fs d) =  (A qs bs q0 fs' d)
           where fs' = filter (\q -> not (elem q fs)) qs

difference :: (Eq q, Eq r, Eq b) => DFSA q b -> DFSA r b -> DFSA (q,r) b
difference a1 a2 = intersection a1 (complement a2)

-- I added a few more functions.

-- 'equivalent a1 a2' returns TRUE only if L(a1) = L(a2). 
equivalent :: (Ord q, Eq q, Eq r, Ord r, Eq b, Ord b) => DFSA q b -> DFSA r b -> Bool
equivalent a1 a2 = (subset a1 a2) && (subset a2 a1) 

-- 'subseteq a1 a2' returns TRUE only if L(a1) is a subset of L(a2). 
subset :: (Ord q, Eq q, Eq r, Ord r, Eq b, Ord b) => DFSA q b -> DFSA r b -> Bool
subset a1 a2 = isEmpty $ intersection a1 (complement a2)

--`isEmpty a' returns TRUE only if L(a) is the emptyset. This is the
-- case whenever the reachable states of overlap with any final
-- states.
isEmpty :: Ord q => Ord b => DFSA q b -> Bool
isEmpty a = null (List.intersect (finals a) (reachableStates a))

-- reachableStates takes a DFSA a and returns all the states reachable
-- from the initial state of a
reachableStates :: Ord q => Ord b => DFSA q b -> [q]
reachableStates a = closure [start a] f
                    where
                      d = delta a
                      successorStates q = map (d q) (sigma a)
                      f qs = nub $ qs ++ concat (map successorStates qs)

-- closure is a higher-order, polymorphic function which successively
-- constructs an object by applying a function to it until the objects
-- no longer changes.
closure :: Eq a => a -> (a -> a) -> a
closure object f 
  | object == object' = object
  | otherwise = closure object' f
  where object' = f object


