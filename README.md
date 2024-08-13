# Haskell-Set
This project involves designing a datatype that represents a finite set of elements.

## Data Type Definition
```haskell
data Set a = 
  Empty | Node (Set a) a (Set a) 
  deriving (Show)
```
In this project, I used a BST approach ensuring elements are sorted to maintain set properties.
Quickcheck functions were used to check my implementations.

Key Functions
* toList: Converts the set to a sorted list.
* fromList: Creates a set from a list, removing duplicates.
* empty: Represents the empty set.
* null: Checks if the set is empty.
* singleton: Creates a set with a single element.
* insert: Inserts an element into the set, maintaining BST properties.
* union: Joins two sets, ensuring no duplicates.
* intersection: Returns common elements between two sets.
* difference: Returns elements in one set but not the other.
* member: Checks if an element is in the set.
* cardinality: Returns the number of elements in the set.
* mapSet: Applies a function to every element in the set.
* setfoldr: Right fold over the set.
* removeset: Removes element 'x' from set.
* powerset: Finds the powerset of a single set.
