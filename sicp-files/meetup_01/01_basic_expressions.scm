(display 43)
(newline)

(display "Hello, world!\n")

(display (+ 1 1))
(newline)

(display (+ 137 349))
(newline)

(display (* 5 99))
(newline)

(display (/ 100 2))
(newline)

(display (+ 1 2 3 4))
(newline)

(display (* 1 2 3 4))
(newline)

;;nested combinations

(display
  (+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))
)
;;57
(newline)

(display
  (+ (* 3
      (+ (* 2 4)
         (+ 3 5)))
   (+ (- 10 7)
      6))
)
;;57
(newline)
