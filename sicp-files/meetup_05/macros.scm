(define-syntax demonstrate
  (syntax-rules ()
    ((_ form) (begin
                (display 'form)
                (display " => ")
                (display form)
                (newline)))))

(define-macro (our-let bindings . body)
  `((lambda ,(map car bindings)
      ,@body)
    ,@(map cadr bindings)))

(demonstrate (our-let ((a 1)
                       (b 2))
               (+ a b)))

(define-macro (our-let* bindings . body)
  (if (nil? bindings)
      `((lambda ()
          ,@body))
      `((lambda ,(list (caar bindings)) ;; caar accesses the first element of the first element of its argument
          (our-let* ,(cdr bindings) ,@body))
        ,(cadar bindings)))) ;; cadar accesses the second element of the first element of its argument

(demonstrate (our-let* ((a 1)
                        (b (+ 1 a))
                        (c (+ a b)))
               (+ a b c)))
