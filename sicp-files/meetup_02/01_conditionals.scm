;; Exercise 1.3
;;  Define a procedure that takes three numbers as arguments 
;; and returns the sum of the squares of the two larger numbers. 

 (define (square x) (* x x)) 
  
 (define (squareSum x y) (+ (square x) (square y))) 
  
 (define (sumOfLargestTwoSquared x y z) 
   (cond ((and (>= (+ x y) (+ y z)) (>= (+ x y) (+ x z))) (squareSum x y)) 
         ((and (>= (+ x z) (+ y z)) (>= (+ x z) (+ x y))) (squareSum x z)) 
         (else (squareSum y z)) 
   ) 
 ) 

(display (sumOfLargestTwoSquared 1 2 3))
(newline)

(display (sumOfLargestTwoSquared 12 0 1))
(newline)

(display (sumOfLargestTwoSquared 8 2 0))
(newline)

;; Exercise 1.4
(define (mystery a b)
  ((if (> b 0) + -) a b))

(display (mystery 8 2))
(newline)

(display (mystery 2 -3))
(newline)

(display (mystery -2 -3))
(newline)

;; So, mystery could be named sum_a_abs_b!

;; 1.1.8



