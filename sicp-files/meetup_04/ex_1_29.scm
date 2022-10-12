(define-syntax demonstrate
  (syntax-rules ()
    ((_ form) (begin
                (display 'form)
                (display " => ")
                (display form)
                (newline)))))

;; ex 1.29 
;; Define a procedure that takes as arguments f, a, b, and n 
;; and returns the value of the integral, computed using Simpson’s Rule. 
;; Use your procedure to integrate cube between 0 and 1 (with n = 100 and n = 1000), 
;; and compare the results to those of the integral procedure shown above.

;; this implementation assumes n is even!
  
;; procedure to get the cube of a number x
(define (cube x) (* x x x)) 
 
;; procedure to increment a number n by 1
(define (inc n) (+ n 1)) 
 
;; procedure to sum a series 
(define (sum term a next b) 
  (if (> a b) 
      0 
      (+ (term a) 
         (sum term (next a) next b)))) 
 
;; procedure to calculate the approximate value of a definite 
;; integral using Simpson's Rule
(define (simpson-integral f a b n) 

  ;; h = (b−a) / n
  (define h (/ (- b a) n)) 

  ;; y_sub_k = f(a+kh)
  (define (yk k) 
    (f (+ a (* h k)))) 

  ;; simpson term -- need to oscillate between 4 and 2
  ;;                 unless k is 0 or n, in which case 
  ;;                 the term is 1
  (define (simpson-term k) 
    (* (cond ((or (= k 0) (= k n)) 1) 
             ((odd? k) 4) 
             (else 2)) 
       (yk k))) 

  ;; putting it all together as Simpson's Rule
  (* (/ h 3) (sum simpson-term 0 inc n))) 

(demonstrate (simpson-integral cube 0 1 1))
(demonstrate (simpson-integral cube 0 1 10))
(demonstrate (simpson-integral cube 0 1 100))
(demonstrate (simpson-integral cube 0 1 1000))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(demonstrate (integral cube 0 1 0.01))
(demonstrate (integral cube 0 1 0.001))
(demonstrate (integral cube 0 1 0.0001))
(demonstrate (integral cube 0 1 0.00001))

;; for ease of intuiting the integral approximation, a couple of integrals below

;; y = mx + b
(define (line m x b) 
  (+ b (* m x)))

;; m = 1, x = 1
(demonstrate (line 1 1 0))
;; m = 1, x = 4
(demonstrate (line 1 4 0))
;; m = 1.5, x = 10
(demonstrate (line 1.5 10 0))

;; function y = 2x + 0
(define (line_2 x) (line 2 x 0))
(demonstrate (line_2 10))

(demonstrate (simpson-integral line_2 0 10 1))
(demonstrate (simpson-integral line_2 0 10 10))
(demonstrate (simpson-integral line_2 0 10 100))

(demonstrate (integral line_2 0 10 0.01))
(demonstrate (integral line_2 0 10 0.001))
(demonstrate (integral line_2 0 10 0.0001))

;;  y = -x^2 + 2
(define (negative_square_plus_2 x) 
  (+ 2 (* -1 (* x x))))

(demonstrate (negative_square_plus_2 0))
(demonstrate (negative_square_plus_2 1))
(demonstrate (negative_square_plus_2 2))

(demonstrate (simpson-integral negative_square_plus_2 -1 1 1))
(demonstrate (simpson-integral negative_square_plus_2 -1 1 10))
(demonstrate (simpson-integral negative_square_plus_2 -1 1 100))

(demonstrate (integral negative_square_plus_2 -1 1 0.01))
(demonstrate (integral negative_square_plus_2 -1 1 0.001))
(demonstrate (integral negative_square_plus_2 -1 1 0.0001))
