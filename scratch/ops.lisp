;; map
(defun my-map (l f)
  (if (null l)
    nil
    (cons (funcall f (car l))
          (my-map (cdr l) f))))

(my-map '(1 2 3) #'1+)

;; filter
(defun my-filter (l f)
  (cond ((null l) nil)
        ((funcall f (car l))
         (cons (car l)
               (my-filter (cdr l) f)))
        (t
          (my-filter (cdr l) f))))

(my-filter '(2 () 3 () 4 ()) #'numberp)

;; fold
(defun fold (acc l f)
  (if (null l)
    acc
    (fold (funcall f acc (car l))
          (cdr l)
          f)))
