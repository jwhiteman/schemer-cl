(defun member? (a lat)
  (if (null lat)
    nil
    (or (eq (car lat) a)
        (member? a (cdr lat)))))

(defun intersect (s1 s2)
  (cond ((null s1) nil)
        ((member? (car s1) s2)
         (cons (car s1)
               (intersect (cdr s1) s2)))
        (t
          (intersect (cdr s1) s2))))

(intersect '(foo bar and bazz) '(bar quux and bizzz))

(defun intersectall (l-set)
  (if (null (cdr l-set))
    (car l-set)
    (intersect (car l-set)
               (intersectall (cdr l-set)))))

(intersectall '((a b c) (c a d e) (x y a z) (d e f g a)))

(intersect '(a b c)
           (intersect '(c a d e)
                      (intersect '(x y a z) '(d e f g a))))

(+ 1
   (+ 2
      (+ 3 0)))
