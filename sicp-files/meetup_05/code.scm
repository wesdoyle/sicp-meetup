(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g)
          (/ d g))))
;; Original Implementation, without GCD calculation:
;; (define (make-rat n d)
;;   (cons n d))

(define (numer rat)
  (car rat))

(define (denom rat)
  (cdr rat))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat rat)
  (display (numer rat))
  (display "/")
  (display (denom rat))
  (newline))

;; Examples:
(define-syntax demonstrate
  (syntax-rules ()
    ((_ form) (begin
                (display 'form)
                (display " => ")
                (display form)
                (newline)))))

(define (show-rat rat)
  "Outputs the result of `print-rat' as a string"
  ;; `parameterize' is how you interact with dynamic variables in Scheme.
  ;; In this case, it overrides `current-output-port' (the port to which
  ;; display writes by default) with the port returned by
  ;; `open-output-string'. -- we can then grab the output as a
  ;; string from that port using `get-output-string'.
  ;; More info about ports: https://scheme.com/tspl4/io.html
  ;;
  ;; Guile (and maybe other Schemes) has a `with-output-to-string'
  ;; which abstracts this and results in somewhat cleaner code
  (parameterize ((current-output-port (open-output-string)))
    (print-rat rat)
    (get-output-string (current-output-port))))


(define one-half (make-rat 1 2))

(demonstrate
 (show-rat one-half))

(define one-third (make-rat 1 3))

(demonstrate
 (show-rat (add-rat one-half one-third)))

(demonstrate
 (show-rat (mul-rat one-half one-third)))

(demonstrate
 (show-rat (add-rat one-third one-third)))

(demonstrate
  (equal-rat? (make-rat 1 5) (make-rat 4 20)))
