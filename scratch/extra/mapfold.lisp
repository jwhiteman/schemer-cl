;; fold v1
(defun fold (f l)
  (labels ((_ (acc l)
           (if (null l)
             acc
             (_ (funcall f acc (car l))
                (cdr l)))))
    (if (null l)
      nil
      (_ (car l) (cdr l)))))

(fold #'+ '(1 2 3))

;; mapfold v1
(defun mapfold (f l)
  (labels ((mapfold-helper (l acc)
              (if (null l)
                '()
                (let ((res (funcall f acc (car l))))
                  (cons res
                        (mapfold-helper (cdr l) res))))))
    (mapfold-helper l nil)))

(mapfold #'(lambda (acc n)
             (if (null acc)
               n
               (+ acc n)))
         '(1 1 1))

;; mapfold v2 (using fold)
