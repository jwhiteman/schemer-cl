;; 1. TOYS

; this is lisp's atom test...but the logic for LS is different (see atom?)
(atom 42)

(defun atom? (x)
  (not (listp x)))

; example of quote
(quote ())

; reminder nil is the empty list
(cons 1 nil)

; reminder: null is the test
(null nil)

; test for list
(listp '(1 2 3))

; cons, car, cdr, eq
