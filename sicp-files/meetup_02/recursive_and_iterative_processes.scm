;;; NOTE: this file is not a script. To run the functions, it will need to be
;;; loaded into a REPL or script
;;;
;;; Assumed Scheme implementation is GNU Guile

;;; Actual factorial procedure implementations
(define (factorial-recursive n)
  (if (eq? n 1)
      1
      (* n (factorial-recursive (- n 1)))))

(define (factorial-iterative n)
  (define (fact-iter product counter max-count)
    (if (> counter max-count)
        product
        (fact-iter (* counter product)
                   (+ counter 1)
                   max-count)))
  (fact-iter 1 1 n))

;;; Demonstration Code
(define (factorial-recursive-demo no)
  "A recursive implementation of factorial that attempts to demonstrate the
expansions at each level of recursion as well as the final expands before reduction"

  (define (repeat-string n str)
    "Constructs a string with `str' repeated `n' times"
    (case n
      ((0) "")
      ((1) str)
      ;; This also happens to be a recursive process 🙂
      (else (string-append str (repeat-string (- n 1) str)))))

  (define (indent n)
    "Indentation utility"
    (display (repeat-string n "|    ")))

  (let ((indent-level 0))
    ;; This is essentially the same as `factorial-recursive', but modified with some messy
    ;; display code so that we can (hopefully) gain insight into the recursive process
    (define (factorial-recursive-impl n)
      (indent indent-level)
      (if (= n 1)
          (begin
            (indent 1)
            (newline)
            (indent indent-level)

            (display 1) ;; Printing the result of this operation in the recursive process

            (newline)
            (indent (+ 1 indent-level))
            (newline)

            1)
          (begin
            (newline)
            (indent indent-level)

            ;; Print a nice version of the list returned below
            (display `(* ,n (factorial-recursive ,(- n 1))))

            (set! indent-level (+ 1 indent-level))
            (newline)

            ;; Instead of returning the actual factorial, this builds a list representing
            ;; the chain of _deferred operations_ in the _recursive process_ of computing the
            ;; factorial of n that would be created in the substitution model.
            `(* ,n ,(factorial-recursive-impl (- n 1))))))
    (let ((res (factorial-recursive-impl no)))
      ;; Print the final chain of operations described above
      (display res)

      (newline)
      (newline)
      (display "Result:\n")
      ;; Evaluate the expanded list returned by `factorial-recursive-impl'
      ;; NOTE: if you're not using Guile, you may need to use `eval'. Different
      ;; implementations seem to have different requirements here
      (primitive-eval res))))

(define (factorial-iterative-demo n)
  "An attempt to display each operation in the iterative process for computing the factorial
of `n'"
  ;; Print first level
  (display `(factorial-iterative 1 1 ,n))
  (factorial-iterative-impl 1 1 n))

;; This is essentially the same as `factorial-iter' inside of `factorial-iterative', but
;; modified with some messy display code so that we can (hopefully) gain insight into the
;; iterative process
(define (factorial-iterative-impl product counter max-count)
  (if (> counter max-count)
      (begin
        (newline)
        (newline)
        product)
      (let (;; The next call expression without the arguments simplified
            (expr `(factorial-iterative (* ,counter ,product)
                                        (+ ,counter 1)
                                        ,max-count))
            ;; The next call expression *with* the arguments simplified
            (simplified `(factorial-iterative ,(* counter product)
                                              ,(+ counter 1)
                                              ,max-count)))

        ;; The idea with displaying both the unsimplified and simplified calls to
        ;; `factorial-iterative' is to show that each step in the process produces
        ;; the full, concrete step required to continue in the next step of the process.
        ;; The are no deferred computations, and there is no implicit state behind
        ;; each step.

        ;; It's important to note the difference between a recursive *process* and a recursive
        ;; *procedure*. While this procedure calls itself--making it a recursive procedure--it
        ;; computes the factorial of n using an iterative process

        (display "\n expands to\n")
        (display expr)
        (display "\n collapses to\n")
        (display simplified)

        ;; NOTE: if you're not using Guile, you may need to use `eval'. Different
        ;; implementations seem to have different requirements here
        (primitive-eval (cons factorial-iterative-impl (cdr expr))))))
