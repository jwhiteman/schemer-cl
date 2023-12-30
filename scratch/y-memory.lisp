;; 1. start with the regular, recursive definition
(defun fac (n)
  (if (zerop n)
    1
    (* n (fac (1- n)))))

;; 2. make it a lambda. use ? where the old call was
(lambda (n)
  (if (zerop n)
    1
    (* n (? (1- n)))))

;; 3. bright idea: pass it in as f
(lambda (f)
  (lambda (n)
    (if (zerop n)
      1
      (* n (funcall f (1- n))))))

;; 4. add two recursive function builders (funcall f f)
(funcall #'(lambda (f)
             (funcall f f))
         (lambda (f)
           (lambda (n)
             (if (zerop n)
               1
               (* n (funcall
                             (funcall f f)
                             (1- n)))))))

;; 4 test: funcall #4 above with an arg. works!
(funcall
  (funcall #'(lambda (f)
               (funcall f f))
           (lambda (f)
             (lambda (n)
               (if (zerop n)
                 1
                 (* n (funcall
                               (funcall f f)
                               (1- n)))))))
  10)

(funcall #'(lambda (f)
             (funcall f f))
         (lambda (f)
           (lambda (n)
             (if (zerop n)
               1
               (* n (funcall (funcall f f) (1- n))
                  )))))

(funcall #'(lambda (f)
             (funcall f f))
         (lambda (f)
           (lambda (n)
             (if (zerop n)
               1
               (* n (funcall m (1- n))
                  )))))
