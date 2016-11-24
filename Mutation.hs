{- Assignment 3 - Memory and Mutation

This file contains the code responsible for working with association lists,
which you will use as the data structure for storing "mutable" data.
-}

-- **YOU MUST ADD ALL FUNCTIONS AND TYPES TO THIS LIST AS YOU CREATE THEM!!**
module Mutation (
    Mutable, get, set, def,
    Memory, Pointer,
    Value,
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


-- Type class representing a type which can be stored in "Memory".
class Mutable a where
    -- Look up a value in memory referred to by a pointer.
    get :: Memory -> Pointer a -> a

    -- Change a value in memory referred to by a pointer.
    -- Return the new memory after the update.
    set :: Memory -> Pointer a -> a -> Memory

    -- Create a new memory location storing a value, returning a new pointer
    -- and the new memory with the new value.
    -- Raise an error if the input Integer is already storing a value.
    def :: Memory -> Integer -> a -> (Pointer a, Memory)

-- Question 2

get_bool (BoolVal value) = value

instance Mutable Bool where
    get mem (P pointer_val) = if inA mem pointer_val then (get_bool (lookupA mem pointer_val))
                              else error "pointer value not in memory!"

    set mem (P pointer_val) value = updateA mem (pointer_val, (BoolVal value))
    def mem pointer_val value = if inA mem pointer_val then error "Already in Memory!"
                                else
                                (
                                  (P pointer_val),
                                  insertA mem (pointer_val, (BoolVal value))
                                )

get_integer (IntVal value) = value

instance Mutable Integer where
  get mem (P pointer_val) = if inA mem pointer_val then (get_integer (lookupA mem pointer_val))
                           else error "pointer value not in memory!"
  set mem (P pointer_val) value = updateA mem (pointer_val, (IntVal value))
  def mem pointer_val value = if inA mem pointer_val then error "Already in Memory!"
                              else
                              (
                                (P pointer_val),
                                insertA mem (pointer_val, (IntVal value))
                              )
