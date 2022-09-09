;; (define (<name> <formal parameters>) <body>)

(define (square x) (* x x))

(display
  (square 21)
)
(newline)

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(display
    (sum-of-squares 3 4)
)
(newline)

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(display
    (f 5)
)
(newline)

;; Exercise 1.5

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;; This will hang forever!
 (display
   (test 0 (p))
 )

;; This will also hang forever!
 (display
   (test 1 (p))
 )

 (display '...')

