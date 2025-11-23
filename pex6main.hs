-- pex6.hs 
-- unKnot Haskell

-- name: Tasha Shusterman

{- DOCUMENTATION: None
-}

unKnot :: [(Char, Char)] -> String
unKnot tripCode
   | null tripCode = "unknot"
   | isType1 tripCode = unKnot (removeType1 tripCode) -- check/remove type 1
   | isType1 (wrap tripCode) = unKnot (removeType1 (wrap tripCode))
   | isType2 tripCode = unKnot (removeType2 tripCode) -- check/remove type 2
   | isType2 (wrap tripCode) = unKnot (removeType2 (wrap tripCode))
   | otherwise = "tangle - resulting trip code: " ++ (show tripCode)

wrap :: [(Char, Char)] -> [(Char, Char)]
wrap [] = []
wrap (x:xs) = xs ++ [x]


-- TESTED AND WORKS!
-- function to check if it is a type 1 move
isType1 :: [(Char, Char)] -> Bool
isType1 [_] = False -- need [_] because if there is only 1 value left, you know it will return False
isType1 (x:y:ys) 
   | fst x == fst y = True --returns whole list if found, twist will be at 1st and 2nd values
   | otherwise = isType1 (y:ys) -- gives you the whole list after the first tuple to look recursively



-- NEED TO ADD WRAP AROUND :(
-- function to check if it is a type 2
isType2 :: [(Char, Char)] -> Bool
isType2 [_] = False
isType2 (x:y:ys)
   | snd x == snd y = isType2Help x y ys --check to see if the 2nd value in the tuple is the same, if yes... call helper function to find the fst values in the rest of the list
   | otherwise = isType2 (y:ys)

-- TESTED AND WORKS!
-- Helper function to find a match for first twist to make it a type 2
isType2Help :: (Char,Char) -> (Char,Char) -> [(Char, Char)] -> Bool
isType2Help _ _ [_] = False
isType2Help a b (x:y:ys)
    | fst a == fst x && fst b == fst y   = True -- twists match, return true!
    | fst a == fst y && fst b == fst x   = True -- twists match, return true!
    | otherwise = isType2Help a b (y:ys) -- otherwise call it again recursively


-- TESTED AND WORKS!
-- function to remove a type 1
removeType1 :: [(Char, Char)] -> [(Char, Char)]
removeType1 [a] = [a] -- want to return the function if there is only 1 value left, otherwise loop will break
removeType1 (x:y:ys) 
   | isType1 [x,y] = ys -- if there is a type 1 in first 2 values of list, return rest of list w/o those
   | otherwise = x : removeType1 (y:ys) -- call function again recursively on rest of list after 1st value if not found


-- TESTED AND WORKS!
-- function to remove a type 2
removeType2 :: [(Char, Char)] -> [(Char, Char)]
removeType2 [] = []
removeType2 [a] = [a]
removeType2 (x:y:ys) 
   | isType2Help x y ys = removeTuple x y ys


--TESTED AND WORKS!
-- helper for removeType2, removes a tuple from the list given a letter of the knot
removeTuple :: (Char,Char) -> (Char,Char) -> [(Char, Char)] -> [(Char, Char)]
removeTuple _ _ [a] = [a]
removeTuple _ _ [] = []
removeTuple a b (x:y:ys)
   | fst a == fst x && fst b == fst y = ys -- if knot to remove matches, remove the tuples from the list
   | otherwise = removeTuple a b ys -- if not found, call the function again recuers

-- helper function to find count of a list
sumList :: [(Char,Char)] -> Int
sumList [] = 0
sumList (_:xs) = 1 + sumList xs

main :: IO ()
main = do
   let t01 = [('a','o'),('a','u')]
   print("   test case t01 - tripcode: " )
   print(t01)
   print("   result:" ++ unKnot t01)

