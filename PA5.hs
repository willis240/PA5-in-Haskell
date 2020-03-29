-- PA5.hs 
-- William Fisher
-- Framework provided by Glenn G. Chappell
-- March 28, 2020
-- Solutions to Assignment 5 Exercise B

module PA5 where

--collatz
--helper function for collatzCounts
--follows collatz algorithm, increments x each time through
collatz 0 _ = 0
collatz k x
    | k == 1  = x
    | even k  = collatz (k `div` 2) (x+1)
    | otherwise = collatz (3*k + 1) (x+1)

--mapCollatz
--A helper function for collatzCounts. Enables collatz function
--to be called on an array with multiple parameters.
mapCollatz f (x:xs) y = f x y : mapCollatz f xs y

--collatzCounts
--An array of numbers from the collatz function based
--on the index location in said array
collatzCounts = mapCollatz collatz [1..] 0

--findHelp
findHelp (x:xs) (y:ys) c l1 l2 e 
	| (x /= y && e==False && l2 == 0) = Nothing --not in list
	| (l2 == 0 || l1 ==0) = Just c --reached end of list1
	| x == y = findHelp xs ys c (l1-1) (l2-1) True
	| (x/= y && e == True) = Nothing --sublist has extra not in list2
    | otherwise = findHelp (x:xs) ys (c+1) l1 (l2-1) False--keep going 

-- findList
findList [] _ = Just 0
findList _ [] = Just 0
findList (x:xs) (y:ys) =
    findHelp (x:xs) (y:ys) 0 (length (x:xs)) (length(y:ys)) False

--findList :: Eq a => [a] -> [a] -> Maybe Int
--findList _ _ = Just 42  -- DUMMY; REWRITE THIS!!!


-- operator ##
(##) :: Eq a => [a] -> [a] -> Int
_ ## _ = 42  -- DUMMY; REWRITE THIS!!!


-- filterAB
filterAB :: (a -> Bool) -> [a] -> [b] -> [b]
filterAB _ _ bs = bs  -- DUMMY; REWRITE THIS!!!


-- sumEvenOdd
sumEvenOdd :: Num a => [a] -> (a, a)
{-
  The assignment requires sumEvenOdd to be written using a fold.
  Something like this:
    sumEvenOdd xs = fold* ... xs where
        ...
  Above, "..." should be replaced by other code. The "fold*" must be
  one of the following: foldl, foldr, foldl1, foldr1.
-}
sumEvenOdd _ = (0, 0)  -- DUMMY; REWRITE THIS!!!
