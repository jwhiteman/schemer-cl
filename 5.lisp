;; 5
(defun atom? (x) (not (listp x)))

;; (rember* a l)
(defun rember* (a l)
  (cond ((null l) '())
        ((atom? (car l))
         (cond ((eq (car l) a)
                (rember* a (cdr l)))
               (t
                 (cons (car l) (rember* a (cdr l))))))
        (t
          (cons (rember* a (car l))
                (rember* a (cdr l))))))

(rember* 'b '(a b ((b) b) (a (a (a (a b))))))

;; (insertR* new old l)
(defun insertR* (new old l)
  (cond ((null l) '())
        ((atom? (car l))
         (cond ((eq (car l) old)
                (cons old (cons new (insertR* new old (cdr l)))))
               (t
                 (cons (car l)
                       (insertR* new old (cdr l))))))
        (t
          (cons (insertR* new old (car l))
                (insertR* new old (cdr l))))))

(insertR* 'y 'x '(a x (a x () (a ((x))) x)))

;; (occur* a l)
(defun occur* (a l)
  (cond ((null l) 0)
        ((atom? (car l))
         (cond ((eq (car l) a)
                (1+ (occur* a (cdr l))))
               (t
                 (occur* a (cdr l)))))
        (t
          (+ (occur* a (car l))
             (occur* a (cdr l))))))

(occur* 'banana '((banana
                    (split ((((banana ice)))
                            (cream (banana))
                            sherbet))
                    (banana)
                    (bread)
                    (banana brandy))))

;; (subst* n o l)
(defun subst* (n o l)
  (cond ((null l) '())
        ((atom? (car l))
         (cond ((eq (car l) o)
                (cons n (subst* n o (cdr l))))
               (t
                 (cons (car l)
                       (subst* n o (cdr l))))))
        (t
          (cons (subst* n o (car l))
                (subst* n o (cdr l))))))

(subst* 'b 'x '(a x (a x () (a ((x))) x)))

;; (insertL* n o l)
(defun insertL* (n o l)
  (cond ((null l) '())
        ((atom? (car l))
         (cond ((eq (car l) o)
                (cons n (cons o (insertL* n o (cdr l)))))
               (t
                 (cons (car l) (insertL* n o (cdr l))))))
        (t
          (cons (insertL* n o (car l))
                (insertL* n o (cdr l))))))

(insertR* 'y 'x '(a x (a x () (a ((x))) x)))

;; (member* a l)
(defun member* (a l)
  (cond ((null l) nil)
        ((atom? (car l))
         (cond ((eq (car l) a) t)
               (t
                 (member* a (cdr l)))))
        (t
          (or (member* a (car l))
              (member* a (cdr l))))))

(member* 'love '(() (() (()) (((((stuff))))))))
(member* 'love '(() (() (()) (((((love))))))))

;; (eqlist l1 l2)
(defun eqlist (l1 l2)
  (cond ((and (null l1)
              (null l2)) t)
        ((or (null l1)
             (null l2) nil))
        ((and (atom? (car l1))
              (atom? (car l2)))
         (and (eq (car l1) (car l2))
              (eqlist (cdr l1) (cdr l2))))
        ((or (atom? (car l1))
             (atom? (car l2))) nil)
        (t
          (and (eqlist (car l1) (car l2))
               (eqlist (cdr l1) (cdr l2))))))

;; (leftmost l)
(defun leftmost (l)
  (if (atom? (car l))
    (car l)
    (leftmost (car l))))
