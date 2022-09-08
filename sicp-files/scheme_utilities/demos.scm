;; NOTE: there doesn't seem to be a great, cross-impl way to load files... copy this into scripts when necessary
(define-syntax demonstrate
  (syntax-rules ()
    ((_ form) (begin
                (display 'form)
                (display " => ")
                (display form)
                (newline)))))
