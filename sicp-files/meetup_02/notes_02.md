# Section 1.1: Building Abstractions with Procedures (Meetup 02)

### 1.1.6: Conditional Expressions and Predicates


Conditionals give us the power to construct a _case analysis_.

Lisp has a special form for case analysis that takes the following form:

```scheme
(cond (<p1> <e1>)
      (<p2> <e2>)
      ...
      (<pn> <en>))
```

For example, the absolute value function could be defined as:

```scheme
(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))
```

> The predicate <p1> is evaluated first. If its value is false, then <p2> is evaluated. If <p2>'s value is also false, then <p3> is evaluated. This process continues until a predicate is found whose value is true, in which case the interpreter returns the value of the corresponding consequent expression <e> of the clause as the value of the conditional expression. If none of the <p>'s is found to be true, the value of the cond is undefined.

The primitive predicates `<`, `=`, and `>` may be used, as well as `and`, `or`, and `not`

```scheme
(and (> x 5) (< x 10))
```

```scheme
(define (>= x y)
  (or (> x y) (= x y)))
 ```

```scheme
(define (>= x y)
  (not (< x y)))
```

### 1.1.7: Example: Square Roots by Newton's Method

> The contrast between function and procedure is a reflection of the general distinction between describing properties of things and describing how to do things, or, as it is sometimes referred to, the distinction between declarative knowledge and imperative knowledge.


Newton's method for estimating a square root:
"Whenever we have a guess `y` for the value of the square root of a number `x`,
we can perform a simple manipulation to get a better guess by averaging `y` with `x/y`"

```scheme
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
```

Notice that we use _recursion_ here in calling a procedure without needing a looping construct.

In example 1.6, note that re-defining a "special form" does not necessarily behave according to the same rules the special form does - e.g. in this example, the redefined expression runs in applicative order. Remember that Lisp
uses applicative order!


### 1.1.8: Procedures as Black-Box Abstractions

A procedure definition should be able to "suppress detail."

The users of the procedure may not have written the procedure themselves, but may have obtained it from another programmer as a black box. A user should not need to know how the procedure is implemented in order to use it.

As a consequence, parameter names of a procedure must be local to the body of that procedure.

E.g. the following should be indistinguishable:

```scheme
(define (square x) (* x x))
(define (square y) (* y y))
```

> A formal parameter of a procedure has a very special role in the procedure definition, in that it doesn't matter what name the formal parameter has. Such a name is called a bound variable, and we say that the procedure definition binds its formal parameters.

>  If a variable is not bound, we say that it is free. In a procedure definition, the bound variables declared as the formal parameters of the procedure have the body of the procedure as their scope. 


```scheme
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
```

In the above example, `guess` and `x` are bound variables, whereas `<`, `-`, `abs` and `square` are free.

- The meaning of `good-enough?` is independent from the names of its bound variables.
- The meaning of `good-enough?` is dependent on the names of its free variables!

#### Block structure and lexical scoping

```scheme
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
```

Note that `x` gets its value from the argument with which the enclosing procedure `sqrt` is called.
