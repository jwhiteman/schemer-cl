;; 9
;; looking
;; keep-looking
;; eternity
;; shift
;; y

(let ((Y
        (lambda (st)
          (funcall #'(lambda (pg) (funcall pg pg))
                   (lambda (f)
                     (funcall st (lambda (x)
                                   (funcall (funcall f f) x)))))))
      (st
        (lambda (f)
          (lambda (n)
            (if (zerop n)
              1
              (* n (funcall f (1- n))))))))
  (funcall (funcall #'Y st) 10))
