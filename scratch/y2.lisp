(defun fact (n)
  (if (zerop n)
    1
    (* n (fact (1- n)))))

(lambda (st)
  (funcall #'(lambda (pg) (funcall pg pg))
           #'(lambda (f)
               (funcall st #'(lambda (n)
                               (funcall (funcall f f) n))))))

(funcall
  (funcall
    #'(lambda (st)
        (funcall #'(lambda (pg) (funcall pg pg))
                 #'(lambda (f)
                     (funcall st #'(lambda (n)
                                     (funcall (funcall f f) n))))))
    #'(lambda (f)
        (lambda (n)
          (if (zerop n)
            1
            (* n (funcall f (1- n)))))))
  10)
