{- Assignment 3 - Memory and Mutation
This file contains the code responsible for working with PART 5
-}

-- **YOU MUST ADD ALL FUNCTIONS AND TYPES TO THIS LIST AS YOU CREATE THEM!!**
module CompoundMutation (
    Mutable, get, set, def,
    Memory, Pointer(..),
    Value(..), StateOp(..),
    (>~>), (>>>), returnVal, alloc, free,
    Person(..), (@@), age, isStudent
    )
    where

import AList (AList,lookupA,insertA,updateA,inA,getKeys,removeA)

-- A type representing the possible values stored in memory.
data Value = IntVal Integer |
             BoolVal Bool
             deriving Show


-- A type representing a container for stored "mutable" values.
type Memory = AList Integer Value

-- A type representing a pointer to a location in memory.
data Pointer a = P Integer | PersonPointer Integer | AgePointer Integer | StudentPointer Integer


-- Question 4

data StateOp a = StateOp (Memory -> (a, Memory))

runOp :: StateOp a -> Memory -> (a, Memory)
runOp (StateOp op) mem = op mem


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
    def :: Integer -> a -> StateOp (Pointer a)

    -- Question 7
    -- assigns an unused key to value a to be put into Memory, returns pointer used
    alloc :: a -> StateOp (Pointer a)

    -- Question 8
    -- remove the pointer and value at a and return StateOp with new mem
    free :: Pointer a -> StateOp ()

-- Question 2

get_bool :: Value -> Bool

get_bool (BoolVal value) = value

instance Mutable Bool where
    get (P pointer_val) = (StateOp (\mem -> (if inA mem pointer_val then (get_bool (lookupA mem pointer_val))
                                             else error "pointer value not in memory!", mem)))
    get (StudentPointer pointer_val) = (StateOp (\mem -> (if inA mem pointer_val then (get_bool (lookupA mem pointer_val))
                                                 else error "pointer value not in memory!", mem)))
    set (P pointer_val) value = (StateOp
                                  (\mem -> (value, updateA mem (pointer_val, (BoolVal value))))
                                )
    def pointer_val value = (StateOp
                              (\mem ->
                                if inA mem pointer_val then error "Already in Memory!"
                                else
                                (
                                  (P pointer_val),
                                  insertA mem (pointer_val, (BoolVal value))
                                )
                              )
                            )

    alloc value = (StateOp
                    (\mem ->
                      let keys = getKeys mem
                          maxmi = (maximum keys + 1)
                          (StateOp f) = def maxmi value
                          (k, newMem) = f mem
                      in (k, newMem)
                    )
                  )

    free (P pointer_value) = (StateOp (\mem -> ((), removeA mem pointer_value)))
    free (StudentPointer pointer_value) = (StateOp (\mem -> ((), removeA mem pointer_value)))

get_integer (IntVal value) = value

instance Mutable Integer where
  get (P pointer_val) = (StateOp (\mem -> (if inA mem pointer_val then (get_integer (lookupA mem pointer_val))
                                           else error "pointer value not in memory!", mem)))
  get (AgePointer pointer_val) = (StateOp (\mem -> (if inA mem pointer_val then (get_integer (lookupA mem pointer_val))
                                           else error "pointer value not in memory!", mem)))
  set (P pointer_val) value = (StateOp
                                (\mem -> (value, updateA mem (pointer_val, (IntVal value))))
                              )
  def pointer_val value = (StateOp
                            (\mem ->
                              if inA mem pointer_val then error "Already in Memory!"
                              else
                              (
                                (P pointer_val),
                                insertA mem (pointer_val, (IntVal value))
                              )
                            )
                          )

  alloc value = (StateOp
                  (\mem ->
                    let keys = getKeys mem
                        maxmi = (maximum keys + 1)
                        (StateOp f) = def maxmi value
                        (k, newMem) = f mem
                    in (k, newMem)
                  )
                )

  free (P pointer_value) = (StateOp (\mem -> ((), removeA mem pointer_value)))
  free (AgePointer pointer_value) = (StateOp (\mem -> ((), removeA mem pointer_value)))

-- Question 4

(>>>) :: StateOp a -> StateOp b -> StateOp b
(StateOp op1) >>> (StateOp op2) = (StateOp (\mem -> let (_, s1) = op1 mem in op2 s1))

(>~>) :: StateOp a -> (a -> StateOp b) -> StateOp b
(StateOp f) >~> g = (StateOp (\mem -> let (x, s1) = f mem
                                          (StateOp newStateOp) = g x
                                      in newStateOp s1
                             )
                    )

-- Question 5

returnVal :: a -> StateOp a
returnVal a = (StateOp (\mem -> (a, mem)))


-- Part 5

--  A type representing a person with two attributes:
-- age and whether they are a student or not.
data Person = Person Integer Bool deriving (Show, Eq)

-- Stores a pointer to age and isStudent
-- data Pointer a b = PersonPointer Integer Integer

-- for every person we have two pointers, one pointer for age, one pointer for isStudent
-- we just plus 1 to get pointer for isStudent
instance Mutable Person where
  get (PersonPointer pointer_val) = (StateOp (\mem ->
                                                (
                                                  if ((inA mem pointer_val) && (inA mem (pointer_val + 1))) then
                                                     Person (get_integer (lookupA mem pointer_val)) (get_bool (lookupA mem (pointer_val + 1)))
                                                     else error "pointer value not in memory!",
                                                 mem
                                                )
                                             )
                                    )
  set (PersonPointer a) (Person age isStudent) = (StateOp
                                                   (\mem ->
                                                    let newMemWithAge = updateA mem (a, (IntVal age))
                                                        newMemWithStu = updateA newMemWithAge ((a + 1), (BoolVal isStudent))
                                                    in ((Person age isStudent), newMemWithStu)
                                                   )
                                                 )
  -- returns the pointer to person_obj once stored in mem/stateop
  -- assumes no pointer at pointer_val and pointer_val + 1
  def pointer_val (Person age isStudent) = (StateOp
                                                  (\mem ->
                                                     if inA mem pointer_val then error "Already in Memory!"
                                                     else
                                                       -- insert
                                                       let memWithAge = insertA mem (pointer_val, (IntVal age))
                                                           memWithIsStudent = insertA memWithAge (pointer_val + 1, (BoolVal isStudent))
                                                       in (
                                                            (PersonPointer pointer_val),
                                                            memWithIsStudent
                                                          )
                                                      )
                                                 )
  -- allocate memory for
  alloc (Person age isStudent) = (StateOp
                                  (\mem ->
                                    let keys = getKeys mem
                                        maxmi = (maximum keys + 1)
                                        (StateOp h) = def maxmi age >>> def (maxmi + 1) isStudent
                                    in (PersonPointer maxmi, mem)
                                  )
                                 )
  -- remove age_pointer and isStudent_pointer
  free (PersonPointer pointer_val) = (StateOp (\mem ->
                                                  let memWithoutAge = removeA mem pointer_val
                                                      memWithoutStudent = removeA mem (pointer_val + 1)
                                                  in ((), memWithoutStudent))
                                       )


-- Returns a
(@@) :: Pointer a -> (Pointer a -> Pointer b) -> Pointer b
(@@) person_pointer attr_func = attr_func person_pointer

-- returns a function which will accept a person_obj and return age
age :: (Pointer a -> Pointer b)
age = (\person_pointer -> let (PersonPointer temp) = person_pointer in AgePointer temp)

isStudent :: (Pointer a -> Pointer b)
isStudent = (\person_pointer -> let (PersonPointer temp) = person_pointer in (StudentPointer (temp + 1)))
