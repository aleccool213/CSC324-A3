{- Assignment 3 - Memory and Mutation

This file contains the code responsible for working with association lists,
which you will use as the data structure for storing "mutable" data.
-}

-- **YOU MUST ADD ALL FUNCTIONS AND TYPES TO THIS LIST AS YOU CREATE THEM!!**
module Mutation (
    Mutable, get, set, def,
    Memory, Pointer,
    Value, StateOp,
    )
    where

import AList (AList, lookupA, insertA, updateA, inA)

-- A type representing the possible values stored in memory.
data Value = IntVal Integer |
             BoolVal Bool
             deriving Show


-- A type representing a container for stored "mutable" values.
type Memory = AList Integer Value

-- A type representing a pointer to a location in memory.
data Pointer a = P Integer

-- Question 4

data StateOp a = StateOp (Memory -> (a, Memory))

runOp :: StateOp a -> Memory -> (a, Memory)
runOp (StateOp op) mem = op mem

--

-- Type class representing a type which can be stored in "Memory".
class Mutable a where
    -- Look up a value in memory referred to by a pointer.
    -- get :: Memory -> Pointer a -> a
    get :: Pointer a -> StateOp a

    -- Change a value in memory referred to by a pointer.
    -- Return the new memory after the update.
    -- set :: Memory -> Pointer a -> a -> Memory
    set :: Pointer a -> a -> StateOp a

    -- Create a new memory location storing a value, returning a new pointer
    -- and the new memory with the new value.
    -- Raise an error if the input Integer is already storing a value.
    -- def :: Memory -> Integer -> a -> (Pointer a, Memory)
    def :: Integer -> a -> StateOp a

-- Question 2

get_bool (BoolVal value) = value

instance Mutable Bool where
    get (P pointer_val) = (StateOp (\mem -> (if inA mem pointer_val then (get_bool (lookupA mem pointer_val))
                                             else error "pointer value not in memory!", mem)))
    set (P pointer_val) value = (StateOp
                                  (\mem -> (value, updateA mem (pointer_val, (BoolVal value))))
                                )
    def pointer_val value = (StateOp
                              (\mem ->
                                if inA mem pointer_val then error "Already in Memory!"
                                else
                                (
                                  value,
                                  insertA mem (pointer_val, (BoolVal value))
                                )
                              )
                            )

get_integer (IntVal value) = value

instance Mutable Integer where
  get (P pointer_val) = (StateOp (\mem -> (if inA mem pointer_val then (get_integer (lookupA mem pointer_val))
                                           else error "pointer value not in memory!", mem)))
  set (P pointer_val) value = (StateOp
                                (\mem -> (value, updateA mem (pointer_val, (IntVal value))))
                              )
  def pointer_val value = (StateOp
                            (\mem ->
                              if inA mem pointer_val then error "Already in Memory!"
                              else
                              (
                                value,
                                insertA mem (pointer_val, (IntVal value))
                              )
                            )
                          )

-- Question 4

(>>>) :: StateOp a -> StateOp b -> StateOp b
(StateOp op1) >>> (StateOp op2) = (StateOp (\mem -> let (_, s1) = op1 mem in op2 s1))

-- (>~>) :: StateOp a -> (a -> StateOp b) -> StateOp b
-- ((StateOp f) >~> g) s = let (x, s1) = f s
--                             newStackOp = f x
--                         in newStackOp s1

-- Question 5

returnVal :: a -> StateOp a
returnVal a = (StateOp (\mem -> (a, mem)))
