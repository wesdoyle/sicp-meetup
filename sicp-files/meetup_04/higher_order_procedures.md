# Notes on Section 1.3: Formulating Abstractions with Higher Order Procedures (Meetup 04)

## Procedures Recap 

Procedures are abstractions that describe compound operations on arbitrary numbers.

i.e.

```lisp
(define (cube x) (* x x x))
```

Is not about the cube of any _particular_ number, but a method for obtaining the cube of any number.

In providing the ability to identify and name such procedures, we expand the expressiveness of the language.

This is still very limited - we need ways to compose procedures to create more expressive abstraction.

## Higher Order Procedures

> Procedures that manipulate procedures are called higher-order procedures.

This section explores procedures that:
- Take procedures as arguments
- Return procedures

### Section 1.3.1 Procedures as Arguments

The example of three structurally-similar procedures

1.) Sum integers between a and b

```lisp
(define (sum-integers a b)
  (if (> a b) 
      0 
      (+ a (sum-integers (+ a 1) b))))
```

2.) Sum cube of integers in the given range 

```lisp
(define (sum-cubes a b)
  (if (> a b) 
      0 
      (+ (cube a) 
         (sum-cubes (+ a 1) b))))
```

3.) Leibniz formula for π

(Often expressed as series (π/4) = 1 − 1/3 + 1/5 − 1/7 + ...)

```lisp
(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) 
         (pi-sum (+ a 4) b))))
```

These procedures do not differ in structure.  It can be generalized as:

```lisp
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
```

where: 

- `sum` is the name of the procedure, 
- `term` is the function of a used to compute the term to be added, and 
- `next` is the function that provides the next value of a
- `a` and `b` are numeric values

This encodes a procedure for calculating the **summation of a series**.

This is conventionally expressed using sigma notation.

Example use for summing cubes:

```lisp
(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))
```

Example use for summing integers:

```lisp
(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))
```

Example use for Leibniz formula for approximation of pi:

```lisp
(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))
```


