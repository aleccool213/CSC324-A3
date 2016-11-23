{- Assignment 3 - Memory and Mutation

This file contains the code responsible for working with association lists,
which you will use as the data structure for storing "mutable" data.
-}
module AList (
    AList,
    lookupA,
    insertA,
    updateA
    )
    where


type AList a b = [(a, b)]

inA :: Eq a => AList a b -> a -> Bool
inA alist key = if (length (filter (\x -> (fst x) == key) alist)) > 0 then True else False

-- | Returns the value in the association list corresponding to the given key.
--   Assumes that the key is in the association list.
lookupA :: Eq a => AList a b -> a -> b
lookupA alist key = let x = (filter (\x -> (fst x) == key) alist)!!0
                    in snd x

-- | Returns a new association list which is the old one, except with
--   the new key-value pair inserted. However, it returns the *same* list
--   if the key already exists in the list.
insertA :: Eq a => AList a b -> (a, b) -> AList a b
insertA alist (key, val) = if (inA alist key) then alist else alist ++ [(key, val)]

-- | Returns a new association list which is the old one, except with
--   the value corresponding to the given key changed to the given new value.
--   However, it returns the *same* list if the key doesn't appear in the list.
updateA :: Eq a => AList a b -> (a, b) -> AList a b
updateA alist (key, val) = foldl
                              (\result current ->
                                if (fst current) == key then result ++ [(key, val)] else result ++ [current]
                              )
                              []
                              alist
