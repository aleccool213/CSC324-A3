{- Assignment 3 - Memory and Mutation

This file contains code which uses the mutation library found in Mutation.hs
-}
module MutationUser (
  pointerTest,
  swap,
  swapCycle
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
swap a b = (StateOp (\mem -> let (StateOp f) = get a
                                 (a_value, newMem) = f mem
                                 (StateOp g) = get b >~> (\b_value -> set a b_value)
                                 (y, newMem2) = g mem
                                 (StateOp h) = set b a_value
                                 (k, newMem3) = h newMem2
                             in ((), newMem3)

              )
            )

-- takes a list of pointers p1,
-- ..., pn, with corresponding values v1, ..., vn, and sets p1's value to v2, p2's value to v3.,
-- etc., and pn's value to v1. This function should not change anything if its argument has length
-- less than 2.
helper [] = StateOp (\mem -> ((), mem))
helper [x] = StateOp (\mem -> ((), mem))
helper (x:y:xs) = swap x y >>> helper (y:xs)

swapCycle :: Mutable a => [Pointer a] -> StateOp ()
swapCycle pointer_list = if (length pointer_list < 3) then StateOp (\mem -> ((), mem))
                         else helper pointer_list
