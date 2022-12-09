(define-syntax demonstrate
  (syntax-rules ()
    ((_ form) (begin
                (display 'form)
                (display " => ")
                (display form)
                (newline)))))

;; Recursive implementation of list length:
;; - Length of a list x is 1 plus length of the cdr of x.
;; - Length of the empty list is 0.

;; Recursive implementation of counting tree leaves 
;; - count-leaves of the empty list is 0.

;; But in the reduction step, where we strip off the car of the list, 
;; we must take into account that the car may itself be a tree whose leaves 
;; we need to count. Thus, the appropriate reduction step is

;; - Count-leaves of a tree x is count-leaves of the car of x 
;; plus count-leaves of the cdr of x.

;; Finally, by taking cars we reach actual leaves, so we need 
;; another base case:

;; - Count-leaves of a leaf is 1.

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define x (cons (list 1 2) (list 3 4)))

(demonstrate (length x))
(demonstrate (count-leaves x))

;; 2.27 Deep Reverse

(define nil '())

(define (reverse items) 
  (define (rev-imp items result) 
    (if (null? items) 
        result 
        (rev-imp (cdr items) (cons (car items) result)))) 
  (rev-imp items nil))

(define (reverse2 items)
  (define (reverse-iter res items*)
    (if (null? items*)
        res
        (reverse-iter (cons (car items*) res)
                      (cdr items*))))
  (reverse-iter (list) items))

(define (deep-reverse t) 
  (if (pair? t) 
      (reverse (map deep-reverse t)) 
      t))

(demonstrate (deep-reverse '(1 2 3 4 5)))
(demonstrate (deep-reverse '(1 2 (3 4) 5 (6 (7 8) 9) 10)))
(demonstrate (deep-reverse '(1 2 (3 4) ( 5 6 7) 8 9 10)))

;; 2.2.3 Sequences as Conventional Interfaces
;; Abstraction provides ways to design systems without needing to 
;; implementation details. Higher-order functions allowed us to
;; capture patterns common in numerical data.

;; We can conceptualize of another type of symmetry in terms of streaming
;; data - data sent through 'signal processing stages' - filters, 
;; maps (transducer), accumulators.
;; even when a procedure may not structurally reflect the data pipeline
;; model, we can conceptualize its functionality in this way

;; we can also implement these procedures using sequences as conventional
;; interfaces.  We can represent signals - the data that flows from one
;; operation to the next - as lists, so that we can leverage list operations
;; as the processing implementation at each stage

;; FILTERING

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate 
                       (cdr sequence))))
        (else  (filter predicate 
                       (cdr sequence)))))

;; (demonstrate (filter odd? (list 1 2 3 4 5)))
;; (demonstrate (filter even? (list 1 2 3 4 5)))
;; (demonstrate (filter even? (list 1 3 5)))

;; ACCUMULATION

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op 
                    initial 
                    (cdr sequence)))))

;; (demonstrate(accumulate + 0 (list 1 2 3 4 5)))
;; (demonstrate(accumulate * 1 (list 1 2 3 4 5)))
;; (demonstrate(accumulate cons nil (list 1 2 3 4 5)))

;; ENUMERATION

(define (enumerate-interval low high)
  (if (> low high)
    nil
    (cons low 
          (enumerate-interval 
            (+ low 1) 
            high))))


(demonstrate (enumerate-interval 2 7))


;; Suppose we have a sequence of personnel records and we want to find 
;; the salary of the highest-paid programmer. Assume that we have a selector 
;; `salary` that returns the salary of a record, and a predicate `programmer?`
;; that tests if a record is for a programmer. Then we can write 


(define 
  (salary-of-highest-paid-programmer
    records)
  (accumulate 
    max
    0
    (map salary
         (filter programmer? records))))

