{- DO NOT CHANGE MODULE NAME, if you do, the file will not load properly -}
module Coursework where

import Data.List 
import qualified Data.Set as HS (fromList, toList)
import Test.QuickCheck
{-
  Your task is to design a datatype that represents the mathematical concept of
  a (finite) set of elements (of the same type). We have provided you with an
  interface (do not change this!) but you will need to design the datatype and
  also support the required functions over sets. Any functions you write should
  maintain the following invariant: no duplication of set elements.

  There are lots of different ways to implement a set. The easiest is to use a
  list. Alternatively, one could use an algebraic data type, wrap a binary
  search tree, or even use a self-balancing binary search tree. Extra marks will
  be awarded for efficient implementations (a self-balancing tree will be more
  efficient than a linked list for example).

  You are **NOT** allowed to import anything from the standard library or other
  libraries. Your edit of this file should be completely self-contained.

  **DO NOT** change the type signatures of the functions below: if you do, we
  will not be able to test them and you will get 0% for that part. While sets
  are unordered collections, we have included the Ord constraint on some
  signatures: this is to make testing easier.

  You may write as many auxiliary functions as you need. Everything must be in
  this file.

  See the note **ON MARKING** at the end of the file.
-}

{-
   PART 1.
   You need to define a Set datatype.
-}

-- you **MUST** change this to your own data type. The declaration of Set a =
-- Int is just to allow you to load the file into ghci without an error, it
-- cannot be used to represent a set.
data Set a = 
  Empty | Node  (Set a ) a (Set a) 
  deriving (Show)
{-
   PART 2.
   If you do nothing else, you must get the toList, fromList and equality working. If they
   do not work properly, it is impossible to test your other functions, and you
   will fail the coursework!
-}

-- toList {2,1,4,3} => [1,2,3,4]
toList :: Ord a => Set a -> [a]
toList Empty = []
toList (Node Empty a Empty) = [a]
toList (Node left x right) = toList left ++ [x] ++ toList right

-- fromList: do not forget to remove duplicates!
fromList :: Ord a => [a] -> Set a
fromList xs = listToBT Empty ( removeDuplicates xs )

listToBT :: (Foldable t, Ord a) => Set a -> t a -> Set a
listToBT xs = foldr Coursework.insert xs 

removeDuplicates :: Eq a => [a] -> [a] 
removeDuplicates = go [] 
  where
    go record [] = []
    go record (x:xs)
      | x `myElem` record = go record xs 
      | otherwise = x : go (record ++ [x]) xs
  
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem y (x:xs) 
  | y == x = True
  | otherwise = myElem y xs 


-- Make sure you satisfy this property. If it fails, then all of the functions
-- on Part 3 will also fail their tests
toFromListProp :: IO ()
toFromListProp =
  quickCheck
    ((\xs -> (HS.toList . HS.fromList $ xs) == (toList . fromList $ xs)) :: [Int] -> Bool)

-- test if two sets have the same elements (pointwise equivalent).
instance (Ord a) => Eq (Set a) where
  s1 == s2 = (toList s1) == (toList s2)

-- you should be able to satisfy this property quite easily
eqProp :: IO ()
eqProp =
  quickCheck ((\xs -> (fromList . HS.toList . HS.fromList $ xs) == fromList xs) :: [Char] -> Bool)

{-
   PART 3. Your Set should contain the following functions. DO NOT CHANGE THE
   TYPE SIGNATURES.
-}

-- the empty set
empty :: Set a
empty = Empty

-- is it the empty set?
null :: Set a -> Bool
null Empty = True
null (Node _ _ _) = False
null (Node left _ right) = False && Coursework.null left && Coursework.null right

setExample :: Set Int
setExample = fromList [6,2,7,4,7,0,2,4,9]
exampleSet :: Set Int
exampleSet = fromList [ 1,2,7,3,7,8,2,3,9]
-- build a one element Set
singleton :: a -> Set a
singleton x = Node Empty x Empty

-- insert an element *x* of type *a* into Set *s* make sure there are no
-- duplicates!
insert :: (Ord a) => a -> Set a -> Set a
insert x Empty = Coursework.singleton x 
insert x (Node left parent right) 
  | x < parent = Node (Coursework.insert x left) parent right
  | x > parent = Node left parent (Coursework.insert x right)
  | otherwise = Node left parent right


-- join two Sets together be careful not to introduce duplicates.
union :: (Ord a) => Set a -> Set a -> Set a
union Empty set2 = set2
union set1 Empty = set1
union set1 set2 = fromList $ (sort(toList set1 ++ toList set2))
-- return, as a Set, the common elements between two Sets
intersection :: (Ord a) => Set a -> Set a -> Set a
intersection Empty _ = Empty
intersection _ Empty = Empty
intersection set1 set2 = fromList $ filter (`member` set2) (toList set1)

pop :: Set Integer
pop = fromList [1,6,0,3,6,9,5,80,2,46,8]
mop :: Set Integer
mop = fromList [1,6,0,3,6,9,20,21,23,24]
-- all the elements in *s1* not in *s2*
-- {1,2,3,4} `difference` {3,4} => {1,2}
-- {} `difference` {0} => {}

difference :: (Ord a) => Set a -> Set a -> Set a
difference Empty _ = Empty
difference set1 Empty = set1
difference set1 set2 = fromList [x | x <- toList set1, not (x `member` set2)]

-- is element *x* in the Set s1?
member :: (Ord a) => a -> Set a -> Bool
member _ Empty = False
member y (Node left x right) 
  | x == y = True
  | x < y = member y left
  | otherwise = member y right 


-- how many elements are there in the Set?
cardinality :: Set a -> Int
cardinality s = setfoldr (\_ x -> x + 1) s 0 

--apply a function to every element in the Set

mapSet :: (Ord b) => (a -> b) -> Set a -> Set b
mapSet _ Empty = Empty

-- right fold a Set using a function *f*
setfoldr :: (a -> b -> b) -> Set a -> b -> b
setfoldr f Empty acc = acc
setfoldr f (Node left x right) acc = 
  setfoldr f left (f x (setfoldr f right acc))     

-- remove an element *x* from the set
-- return the set unaltered if *x* is not present
removeSet :: (Eq a) => a -> Set a -> Set a
removeSet x s = undefined

-- powerset of a set
-- powerset {1,2} => { {}, {1}, {2}, {1,2} }
powerSet :: Set a -> Set (Set a)
powerSet s = undefined

{-
   ON MARKING:

   Be careful! This coursework will be marked using QuickCheck, against
   Haskell's own Data.Set implementation. This testing will be conducted
   automatically via a marking script that tests for equivalence between your
   output and Data.Set's output. There is no room for discussion, a failing test
   means that your function does not work properly: you do not know better than
   QuickCheck and Data.Set! Even one failing test means 0 marks for that
   function. Changing the interface by renaming functions, deleting functions,
   or changing the type of a function will cause the script to fail to load in
   the test harness. This requires manual adjustment by a TA: each manual
   adjustment will lose 10% from your score. If you do not want to/cannot
   implement a function, leave it as it is in the file (with undefined).

   Marks will be lost for too much similarity to the Data.Set implementation.

   Pass: creating the Set type and implementing toList and fromList is enough
   for a passing mark of 40%, as long as both toList and fromList satisfy the
   toFromListProp function.

   The maximum mark for those who use Haskell lists to represent a Set is 70%.
   To achieve a higher grade than is, one must write a more efficient
   implementation. 100% is reserved for those brave few who write their own
   self-balancing binary tree.
-}