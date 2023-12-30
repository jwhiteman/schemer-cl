;; 2.

;; HELPERS
(defun atom? (a)
  (not (listp a)))

;; (LAT? L)
(defun lat? (l)
  (if (null l)
    t
    (and (atom? (car l))
         (lat? (cdr l)))))

;; alternate
(defun lat? (l)
  (cond ((null l) t)
        ((atom? (car l))
         (lat? (cdr l)))
        (t
          nil)))

(lat? nil)
(lat? '(a b c))
(lat? '(a (b) c))


;; (MEMBER? A LAT)
(defun member? (a lat)
  (cond ((null lat) nil)
        ((eq (car lat) a)
         t)
        (t
          (member? a (cdr lat)))))

(member? 'a nil)
(member? 'a '(x y z))
(member? 'a '(x y a z))
