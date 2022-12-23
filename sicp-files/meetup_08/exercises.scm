(define (variable? e)
  "Is e a variable?"
  (symbol? e))

(define (same-variable? v1 v2)
  "Are v1 and v2 the same variable?"
  (and (symbol? v1) (symbol? v2) (eq? v1 v2)))

(define (sum? e)
  "Is e a sum?"
  (and (pair? e) (eq? (car e) '+)))

(define (addend e)
  "Addend of the sum e."
  (cadr e))

(define (augend e)
  "Augend of the sum e."
  (caddr e))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

;; (define (make-sum a1 a2)
;;   (list '+ a1 a2))
;; Updated example which simplifies the sum
(define (make-sum a1 a2)
  "Construct the sum of a1 and a2."
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (product? e)
  "Is e a product?"
  (and (pair? e) (eq? (car e) '*)))

(define (multiplier e)
  "Multiplier of the product e."
  (cadr e))

(define (multiplicand e)
  "Multiplicand of the product e."
  (caddr e))

;; (define (make-product m1 m2)
;;   "Construct the product of m1 and m2."
;;   (list '* m1 m2))
;; Updated example which simplifies the product

(define (make-product m1 m2)
  (cond ((or (=number? m1 0)
             (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list '* m1 m2))))

;;; New procs for exercise 2.56 - Exponentiation
(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))

(define (base exp)
  (cadr exp))

(define (exponent exp)
  (caddr exp))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((and (number? b) (number? e))
         (expt b e))
        (else (list '** b e))))
;;; End new procs for exercise 2.56 - Exponentiation

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        ((exponentiation? exp)
         ;; Doing something like (deriv '(* 2 (** x 3)) 'x)
         ;; produces (* 2 (* 3 (** x 2)))
         ;; Could we modify make-product such that it simplifies when one argument is a constant
         ;; and the other is a product that also has one constant?
         ;; to something like (* 6 (** x 2))
         (make-product
          (exponent exp)
          (make-exponentiation (base exp)
                               (make-sum (exponent exp)
                                         -1))))
        (else (error "unknown expression
                      type: DERIV" exp))))

(define-syntax demonstrate
  (syntax-rules ()
    ((_ form) (begin
                (display 'form)
                (display " => ")
                (display form)
                (newline)))))

(demonstrate (deriv '(+ x 3) 'x))
;; => 1

(demonstrate (deriv '(* x y) 'x))
;; => y

(demonstrate (deriv '(* (* x y) (+ x 3)) 'x))
;; => (+ (* x y) (* y (+ x 3)))
