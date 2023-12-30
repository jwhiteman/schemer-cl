;; map
(defun my-map (f l)
  (if (null l)
    '()
    (cons (funcall f (car l))
          (my-map f (cdr l)))))

(my-map #'1+ '(1 2 3))

;; filter
(defun filter (f l)
  (cond ((null l) '())
        ((funcall f (car l))
         (cons (car l) (filter f (cdr l))))
        (t
          (filter f (cdr l)))))

(filter #'(lambda (x) (> x 2)) '(1 2 3 4 5))

;; fold
(defun my-reduce (f acc l)
  (if (null l)
    acc
    (my-reduce f
          (funcall f acc (car l))
          (cdr l))))

(my-reduce #'(lambda (x y) (+ x y)) 0 '(1 2 3))
