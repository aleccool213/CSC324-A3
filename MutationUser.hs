{- Assignment 3 - Memory and Mutation

This file contains code which uses the mutation library found in Mutation.hs
-}
module MutationUser (
  pointerTest,
  swap
  )
  where

import Mutation (
    get, set, def, Mutable, Pointer(..), Memory, StateOp(..), (>~>), (>>>),
    Value(..)
    )

-- Question 3

-- | Takes a number <n> and memory, and stores two new values in memory:
--   - the integer (n + 3) at location 100
--   - the boolean (n > 0) at location 500
--   Return the pointer to each stored value, and the new memory.
--   You may assume these locations are not already used by the memory.
-- old -- pointerTest :: Integer -> Memory -> ((Pointer Integer), (Pointer Bool), Memory)

-- what is the type annotation here?
pointerTest :: Integer -> StateOp ((Pointer Integer), (Pointer Bool))
pointerTest value = (StateOp (\mem -> let (StateOp f) = (def 100 (value + 3))
                                          (p1, s1) = f mem
                                          (StateOp y) = (def 500 (if value > 0 then True else False))
                                          (p2, s2) = y s1
                                      in ((p1, p2), s2)
                      )
                    )



-- Part 3

-- Question 6
-- takes two pointers and swaps the values they refer to.
swap :: Mutable a => Pointer a -> Pointer a -> StateOp ()
-- swap a b = (StateOp (\mem -> let (StateOp f) = get a
--                                  (x, s1) = f mem
--                                  (StateOp g) = get b
--                                  (y, s2) = g s1
--                                  (StateOp k) = set a x >>> set b y
--                              in ((), (snd (k mem)))
--
--               )
--             )
swap a b = get b >~> (\value -> set a value) >>>
           get a >~> (\value -> set b value) >>>
           StateOp (\mem -> ((), mem))


-- swap :: Mutable a => Pointer a -> Pointer a -> StateOp ()
-- swap a b = let (StateOp x) = get a
-- 					     (StateOp y) = get b
-- 			in StateOp (\mem -> ((), mem))

-- swapCycle :: Mutable a => [Pointer a] -> StateOp ()
-- swapCycle = undefined
