;; Copied from sicp_files/scheme-utilities/demos.scm
(define-syntax demonstrate
  (syntax-rules ()
    ((_ form) (begin
                (display 'form)
                (display " => ")
                (display form)
                (newline)))))

(demonstrate 43)
(newline)

(demonstrate "Hello, world!\n")

(demonstrate (+ 1 1))
(newline)

(demonstrate (+ 137 349))
(newline)

(demonstrate (* 5 99))
(newline)

(demonstrate (/ 100 2))
(newline)

(demonstrate (+ 1 2 3 4))
(newline)

(demonstrate (* 1 2 3 4))
(newline)

;;nested combinations

(demonstrate
  (+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))
)
;;57
(newline)

(demonstrate
  (+ (* 3
      (+ (* 2 4)
         (+ 3 5)))
   (+ (- 10 7)
      6))
)
;;57
(newline)
