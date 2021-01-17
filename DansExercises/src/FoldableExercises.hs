module FoldableExercises where

import           Prelude hiding (replicate, sum, filter, foldr, take, map)
import qualified Data.Foldable as DF (foldr)

data MyList a
  = (:|) a (MyList a)
  | EndOfList
  deriving stock Show
  deriving stock Eq

infixr 5 :|


-- The replicate function creates a list that contains the `x` parameter count `times`.
-- eg. replicate 3 "a" == "a" :| "a" :| "a" :| EndOfList
replicate' :: Int -> a -> MyList a
replicate' count a
  | count > 0 = a :| replicate' (count - 1) a
  | otherwise = EndOfList

-- Write out all the evaluation steps of replicate 3 "a". Put each
-- step on its own line.
-- Here's the start:
-- replicate 3 "a"
-- "a" :| replicate (3 - 1) "a"
-- "a" :| replicate (2 - 1) "a"
-- "a" :| replicate (1 - 1) "a"


-- Implement the take function that returns a list that is only
-- the first n elements of the input list
-- eg. take 2 ('a' :| 'b' :| 'c' :| 'd' :| EndOfList) == ('a' :| 'b' :| EndOfList)
-- eg. take 2 ('a' :| EndOfList) == ('a' :| EndOfList)
take' :: Int -> MyList a -> MyList a
take' count EndOfList = EndOfList
take' count (a :| tail)
  | count > 0 = a :| take' (count - 1) tail
  | otherwise = EndOfList


-- If we combined replicate and take like so:
-- take 2 (replicate 5 "a")
-- Do you think that we would construct a list 5 elements long?
-- Remember that Haskell has lazy evaluation and find out by
-- writing out the evaluation steps. Put each step on its own line.
-- Here's the start:
-- take 2 (replicate 5 "a")
-- take 2 ("a" :| replicate (5 - 1) "a")
-- take 2 ("a" :| a :| EndOfList)


-- Implement `countDownFrom` which counts down to zero from the number you pass in
-- using recursion.
-- eg. countDownFrom 3 == (3 :| 2 :| 1 :| 0 :| EndOfList)
countDownFrom :: Int -> MyList Int
countDownFrom start 
  | start > 0 = start :| countDownFrom (start - 1)
  | otherwise = EndOfList


-- Implement a function that maps any value of a, into a value of b
-- for all the elements of a list. The order of the list should be
-- retained.
-- eg. map (+1) (1 :| 3 :| 5 :| EndOfList) == (2 :| 4 :| 6 :| EndOfList)
map' :: (a -> b) -> MyList a -> MyList b
map' fn EndOfList = EndOfList
map' fn (x :| list) = fn x :| map' fn list


-- Implement a function that filters a list using a predicate function.
-- The list elements should retain their original order
-- eg. filter (/=3) (1 :| 3 :| 5 :| EndOfList) == (1 :| 5 :| EndOfList)
filter' :: (a -> Bool) -> MyList a -> MyList a
filter' predicate EndOfList = EndOfList
filter' predicate (a :| list)
  | predicate a = a :| filter' predicate list
  | otherwise = filter' predicate list

-- Implement a function that takes all the strings in the input list
-- and returns a single list of all the characters
-- eg. charsFromLines ("te" :| "st" :| EndOfList) == 't' :| 'e' :| 's' :| 't' :| EndOfList
charsFromLines :: MyList String -> MyList Char
charsFromLines EndOfList = EndOfList
charsFromLines ((c : cs) :| xs) = concat' (stringToMyListChar c cs) (charsFromLines xs)

concat' :: MyList Char -> MyList Char -> MyList Char
concat' EndOfList y = y
concat' (x :| xs) y = x :| concat' xs y

stringToMyListChar :: Char -> String -> MyList Char
stringToMyListChar c [] = c :| EndOfList
stringToMyListChar c (x:xs) = c :| (stringToMyListChar x xs) 

-- You may have noticed a pattern in your recursive functions so far, especially
-- if you inspect the evaluation steps that you wrote out.
-- Each appends the the remainder of the recursion as "the rest of the list"!
-- Foldr is a generalisation of this technique, where the :| function can
-- be passed in by the caller.
-- Implement foldr below.
-- eg. foldr (\a s -> a + 1 :| s) EndOfList (1 :| 2 :| 3 :| EndOfList) == (2 :| 3 :| 4 :| EndOfList)
foldr' :: (a -> s -> s) -> s -> MyList a -> s
foldr' f s EndOfList = s
foldr' f s (x :| xs) = f x (foldr' f s xs)

fuelRequired x m = (floor (m / 3) - 2) + x

totalFuel xs = foldl fuelRequired 0 xs

-- Reimplement your map function using your new fancy foldr
-- mapUsingFoldr :: (a -> b) -> MyList a -> MyList b
-- mapUsingFoldr fn EndOfList = EndOfList
-- mapUsingFoldr fn (x :| list) = fn x :| foldr' fn EndOfList list

-- Do you think your mapUsingFoldr can be used on infinite lists
-- or very long lists without materialising the whole list?


-- Reimplement your filter function using your foldr
-- filterUsingFoldr :: (a -> Bool) -> MyList a -> MyList a
-- filterUsingFoldr predicate list =
--   undefined


-- Reimplement your charsFromLines using your foldr.
-- HINT: You'll also need DF.foldr to fold over the Strings ([Char])
-- charsFromLinesWithFoldr :: MyList String -> MyList Char
-- charsFromLinesWithFoldr lines =
--   undefined