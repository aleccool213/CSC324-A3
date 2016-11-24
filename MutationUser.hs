{- Assignment 3 - Memory and Mutation

This file contains code which uses the mutation library found in Mutation.hs
-}

import Mutation (
    get, set, def, Mutable, Pointer, Memory, StateOp
    )

-- Question 3

-- | Takes a number <n> and memory, and stores two new values in memory:
--   - the integer (n + 3) at location 100
--   - the boolean (n > 0) at location 500
--   Return the pointer to each stored value, and the new memory.
--   You may assume these locations are not already used by the memory.
pointerTest :: Integer -> StateOp a -> StateOp a
pointerTest value = def 100 (value + 3) >~> \x ->
                            def 500 (if value > 0 then True else False) >~> \y ->
                        get x >>> get y

swap :: Mutable a => Pointer a -> Pointer a -> StateOp ()
swap Pointer a Pointer b = let x = get a
				set a (get b)
				set b x

swapCycle :: Mutable a => [Pointer a] -> StateOp ()

