# Section 1.1: Building Abstractions with Procedures

## 1.1: The Elements of Programming - Notes

> The acts of the mind, wherein it exerts its power over simple
r ideas, are chiefly these three: 1. Combining several simple
> ideas into one compound one, and thus all complex ideas
> are made. 2. The second is bringing two ideas, whether sim-
> ple or complex, together, and seeing them by one another
> so as to take a view of them at once, without uniting them
> into one, by which it gets all its ideas of relations. 3. The
> third is separating them from all other ideas that accom-
> pany them in their real existence: this is called abstraction,
> and thus all its general ideas are made.
> —John Locke, An Essay Concerning Human Understanding
> (1690)

- Every powerful programming language has 3 mechanisms for creating complex ideas from simple ideas:

    1 - **primitive expressions** - represent the simplest entities in the language

    2 - **means of combination** - by which compound elements are built from simpler ones

    3 - **means of abstraction** - by which compound elements can be named and manipulated as units

- In programming, we deal with two kinds of elements: procedures and data
    - later, we'll see there is not much distinction

- This section focuses on the rules for building procedures

### 1.1.1: Expressions

- Numbers are one kind primitve expression in Lisp

```lisp
486
```

- Expressions representing numbers can be combined with primitive operations to form compound expressions

```lisp
(+ 137 349)

(* 1 2 3 4)

(/ 100 2)
```

- These types of expressions, consisting of expressions delimited by space within parens, are called __combinations__.

    - The leftmost element is called the __operator__.
    - The other elements are called the __operands__.

- The convention of placing the operator at the left of the operands is called __prefix notation__.

    - Useful because it is unambiguous
    - Operators can take an arbitrary number of operands
    - Easy to unambiguously combine using nesting

```lisp
(+ (* 3 5) (- 10 6))
(+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))

(+ (* 3
(+ (* 2 4)
(+ 3 5)))
(+ (- 10 7)
6))
```

### 1.1.2: Naming and the Environment

#### Naming

- The name of an element identifies a __variable__ whose __value__ is some object

- In Scheme, things are named using `define`. `define` is our simplest means of abstraction in this language.

- For example, to cause the interpreter to associate the name `size` with the value `2`:

```scheme
(define size 2)
```

- Now, using the name `size` can be used to refer to the value `2`:

```scheme
(* 5 size)
10
```

- Another example:

```scheme
(define pi 3.14159)
(define radius 10)
(* pi (* radius radius))
314.159
(define circumference (* 2 pi radius))
circumference
62.8318
```

- Ultimately, complex programs are constructed by building computational objects of increasing complexity.

#### The Environment 

- The possiblity of associating values with symbols and retrieving that relationship implies the use of __memory__.

- This memory is called the __global environment__.

### 1.1.3: Evaluating Combinations

- The interpreter is following a recursive procedure

    - To evaluate a combination, do the following:
        1. Evaluate the subexpressions of the combination.
        2. Apply the procedure that is the value of the leftmost subexpression (the operator) to the arguments that are the values of the other subexpressions (the operands).

- This process can be represented as a tree.
    - Each combination is represented as a node with branches corresponding to the operator and operands.
    - We can picture the values of the operands as percolating upward in a process called __tree accumulation__ (formally, a __catamorphism__)

- Ultimately, repeated application leads to a point where we're evaluating primitive expressions like numbers and built-in operators or names.
    - the values of numerals are the numbers that they name, 
    - the values of built-in operators are the machine instruction sequences that carry out the corresponding operations, and
    - the values of other names are the objects associated with those names in the environment.

- Note the evaluation rule does not handle _definitions_ e.g.:

```scheme
(define x 3)
```

- Exceptions to the general evaluation rule are called __special forms__.
    - Each special form has its own evaluation rule
    - The various kinds of expressions constitute the syntax of the programming language

### 1.1.4: Compound Procedures

### 1.1.5: The Substitution Model for Procedure Application

### 1.1.6: Conditional Expressions and Predicates

### 1.1.7: Example: Square Roots by Newton's Method

### 1.1.8: Procedures as Black-Box Abstractions

## 1.1: Example Problems
