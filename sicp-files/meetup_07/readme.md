# Meetup 7 Notes

## 2.2.2 Hierarchical Structures

It's useful to be able to generalize sequences such that they can contain 
sequences. 

```scheme
(cons (list 1 2) (list 3 4))
```
is one way to construct the object `((1 2) 3 4)`,
which has a list of elements (1 2) as the first item in the list.

Lists of lists can also be naturally modeled as tree data structures.
Recursion lends itself well to operating on trees, because we can often reduce 
operations to trees to operations on their branches, an to their branches, etc 
- until we've reached the leaves of the tree.

