{- Assignment 3 - Memory and Mutation

This file contains code which uses the mutation library found in Mutation.hs
-}
module MutationUser(pointerTest, swap, swapCycle) where

import Mutation (
    get, set, def, Mutable, Pointer(..), Memory, StateOp(..), (>~>), (>>>),
    )

-- Question 3

-- | Takes a number <n> and memory, and stores two new values in memory:
--   - the integer (n + 3) at location 100
--   - the boolean (n > 0) at location 500
--   Return the pointer to each stored value, and the new memory.
--   You may assume these locations are not already used by the memory.
-- old -- pointerTest :: Integer -> Memory -> ((Pointer Integer), (Pointer Bool), Memory)

-- what is the type annotation here?
pointerTest :: Integer -> StateOp a
pointerTest = undefined
-- pointerTest value = let (x, s1) = def 100 (value + 3)
--                         (y, s2) = def 500 (if value > 0 then True else False)


swap :: Mutable a => Pointer a -> Pointer a -> StateOp ()
swap a b = let (StateOp x) = get a
               (StateOp y) = get b
           in  (set a val) >>> (set b val)

-- swapCycle :: Mutable a => [Pointer a] -> StateOp ()
-- swapCycle = undefined
