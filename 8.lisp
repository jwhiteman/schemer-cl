;; 8
;; (rember-f test? a l)
(defun rember-f (f a l)
  (cond ((null l) '())
        ((funcall f (car l) a)
         (cdr l))
        (t
          (cons (car l)
                (rember-f f a (cdr l))))))

;; eq?-c
(defun eq?-c (a)
  (lambda (x)
    (eq x a)))

;; book gives this; why?
(defun eq?-c (a)
  (function
    (lambda (x)
      (eq x a))))

;; rember-f (v2 curry)
(defun rember-f (test)
  (lambda (a l)
    (cond ((null l) '())
          ((funcall test (car l) a)
           (cdr l))
          (t
            (cons (car l)
                  (funcall (rember-f test) a (cdr l)))))))

;; insertL-f
(defun insert-left (n o l)
  (cond ((null l) '())
        ((eq (car l) o)
         (cons n (cons o
                       (insert-left n o (cdr l)))))
        (t
          (cons (car l)
                (insert-left n o (cdr l))))))

(defun insert-left-f (test?)
  (lambda (n o l)
    (cond ((null l) '())
          ((funcall test? (car l) o)
           (cons n
                 (cons o
                       (funcall (insert-left-f test?) n o (cdr l)))))
          (t
            (cons (car l)
                  (funcall (insert-left-f test?) n o (cdr l)))))))

(funcall (insert-left-f #'eq) 'spicy 'tuna '(savoury tuna sandwich))

;; insertR-f
(defun insertR (n o l)
  (cond ((null l) '())
        ((eq (car l) o)
         (cons (car l)
               (cons n
                 (insertR n o (cdr l)))))
        (t
          (cons (car l)
                (insertR n o (cdr l))))))

(defun insertR-f (test?)
  (lambda (n o l)
    (cond ((null l) '())
          ((funcall test? (car l) o)
           (cons o
                 (cons n
                       (funcall (insertR-f test?) n o (cdr l)))))
          (t
            (cons (car l)
                  (funcall (insertR-f test?) n o (cdr l)))))))

(funcall (insertR-f #'eq) 'z 'y '(a y b y b y b))


;; insert-g
(defun seqR (n o l)
  (cons o (cons n l)))
(defun seqL (n o l)
  (cons n (cons o l)))

(defun insert-g (test? seq)
  (lambda (n o l)
    (cond ((null l) '())
          ((funcall test? (car l) o)
           (funcall seq n o
                    (funcall (insert-g test? seq) n o (cdr l))))
          (t
            (cons (car l)
                  (funcall (insert-g test? seq) n o (cdr l)))))))

(funcall (insert-g #'eq #'seqL) 'z 'y '(a y b y b y b))
(funcall (insert-g #'eq #'seqR) 'z 'y '(a y b y b y b))
(funcall (insert-g #'eq #'(lambda (new old l)
                            (cons new (cons old l))))
         'z
         'y
         '(a y b y b y b))

;; seqS subst
(defun my-subst (new old l)
  (cond ((null l) '())
        ((eq (car l) old)
         (cons new
               (my-subst new old (cdr l))))
        (t
          (cons (car l)
                (my-subst new old (cdr l))))))

(my-subst 'foo 'bar '(bar bazz bar quux))

(defun seqS (n o l)
  (declare (ignore o))
  (cons n l))

(defun my-subst-2 ()
  (insert-g #'eq #'seqS))

(funcall (my-subst-2) 'z 'y '(a y b y b))

;; atom-to-function value
(defun atom? (e)
  (not (listp e)))
(defun operator (nexp)
  (car (cdr nexp)))
(defun 1st-sub-exp (nexp)
  (car nexp))
(defun 2nd-sub-exp (nexp)
  (car (cdr (cdr nexp))))

(defun value (nexp)
  (cond
    ((atom? nexp) nexp)
    ((eq (operator nexp) '+)
     (+ (value (1st-sub-exp nexp))
        (value (2nd-sub-exp nexp))))
    ((eq (operator nexp) '*)
     (* (value (1st-sub-exp nexp))
        (value (2nd-sub-exp nexp))))
    (t
      (expt (value (1st-sub-exp nexp))
            (value (2nd-sub-exp nexp))))))

(value '((2 + 2) * (3 ** 9)))

(defun atom-to-function (x)
  (cond ((eq x '+) #'+)
        ((eq x '*) #'*)
        (t #'expt)))

(defun value-2 (nexp)
  (if (atom? nexp)
    nexp
    (funcall (atom-to-function (operator nexp))
             (value (1st-sub-exp nexp))
             (value (2nd-sub-exp nexp)))))

(value-2 '((2 + 2) * (3 ** 9)))

;; multirember-f
(defun multirember (a lat)
  (cond ((null lat) '())
        ((eq (car lat) a)
         (multirember a (cdr lat)))
        (t
          (cons (car lat)
                (multirember a (cdr lat))))))
(multirember 'c '(a c d c))

(defun multirember-f (test?)
  (lambda (a lat)
    (cond ((null lat) '())
          ((funcall test? (car lat) a)
           (funcall (multirember-f test?) a (cdr lat)))
          (t
            (cons (car lat)
                  (funcall (multirember-f test?) a (cdr lat)))))))

(funcall (multirember-f #'eq) 'c '(a c d c))

;; multirember-eq?
(defun multirember-eq () (multirember-f #'eq))
(funcall (multirember-eq) 'c '(a c d c))

;; multiremberT
(defun eq-tuna? (a) (eq a 'tuna))

(defun multiremberT (test? lat)
  (cond ((null lat) '())
        ((funcall test? (car lat))
         (multiremberT test? (cdr lat)))
        (t
          (cons (car lat)
                (multiremberT test? (cdr lat))))))

(multiremberT #'eq-tuna? '(veggie and tuna sandwich tuna roll))

;; multirember&co
(defun friendo (x y) (cons x (cons y nil)))
(defun multirember-c (a lat co)
  (cond ((null lat)
         (funcall co nil nil))
        ((eq (car lat) a)
         (multirember-c a (cdr lat)
                        #'(lambda (x y)
                            (funcall co x (cons a y)))))
        (t
          (multirember-c a (cdr lat)
                         #'(lambda (x y)
                             (funcall co
                                      (cons (car lat) x)
                                      y))))))

(multirember-c 'c '(a c d c) #'friendo)
(multirember-c 'c '(a c d c) #'(lambda (x y) (declare (ignore x)) (length y)))

(multirember-c 'c '(a c d c e f g c) #'friendo)

;; multiinsertLR
(defun multiinsertLR (new oldL oldR lat)
  (cond ((null lat) '())
        ((eq (car lat) oldL)
         (cons new (cons oldL
                         (multiinsertLR new oldL oldR (cdr lat)))))
        ((eq (car lat) oldR)
         (cons oldR (cons new
                          (multiinsertLR new oldL oldR (cdr lat)))))
        (t
          (cons (car lat)
                (multiinsertLR new oldL oldR (cdr lat))))))

(multiinsertLR 'spicy 'tuna 'super '(tuna sandwich tuna roll super delicious))

;; multiinsertLR&co
(defun friendo (x y z) (cons x (cons y (cons z '()))))

(defun multiinsertLR&co (new oldL oldR lat co)
  (cond ((null lat)
         (funcall co '() 0 0))
        ((eq (car lat) oldL)
         (multiinsertLR&co new oldL oldR (cdr lat)
                           #'(lambda (newlat left right)
                               (funcall co
                                        (cons new (cons oldL newlat))
                                        (1+ left)
                                        right))))
        ((eq (car lat) oldR)
         (multiinsertLR&co new oldL oldR (cdr lat)
                           #'(lambda (newlat left right)
                               (funcall co
                                        (cons oldR (cons new newlat))
                                        left
                                        (1+ right)))))
        (t
          (multiinsertLR&co new oldL oldR (cdr lat)
                            #'(lambda (newlat left right)
                                (funcall co
                                         (cons (car lat) newlat)
                                         left
                                         right))))))

(multiinsertLR&co 'spicy 'tuna 'super '(tuna sandwich tuna roll super delicious) #'friendo)
(multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips) #'friendo)

;; evens-only*
(defun atom? (a) (not (listp a)))

(defun lon*-filter (lon test?)
  (cond ((null lon) '())
        ((and (atom? (car lon))
              (funcall test? (car lon)))
         (lon*-filter (cdr lon) test?))
        ((atom? (car lon))
         (cons (car lon)
               (lon*-filter (cdr lon) test?)))
        (t
          (cons (lon*-filter (car lon) test?)
                (lon*-filter (cdr lon) test?)))))

(lon*-filter '(1 2 3 4 5 6 7 8 9 10) #'oddp)
(defun evens-only* (lon) (lon*-filter lon #'oddp))
(evens-only* '(1 (2) 3 (4 5 (6) 7 (()) 8 (9) 10)))
(evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2))

;; evens-only*&co
(defun atom? (a) (not (listp a)))
(defun friendo (x y z) (cons x (cons y (cons z '()))))

(defun lon*-filter-co (lon test? co)
  (cond ((null lon) (funcall co '() 1 0))
        ((and (atom? (car lon))
              (funcall test? (car lon)))
         (lon*-filter-co (cdr lon)
                         test?
                         #'(lambda (newlist evens odds)
                             (funcall co
                                      newlist
                                      evens
                                      (+ (car lon) odds)))))
        ((atom? (car lon))
         (lon*-filter-co (cdr lon)
                         test?
                         #'(lambda (newlist evens odds)
                             (funcall co
                                      (cons (car lon) newlist)
                                      (* (car lon) evens)
                                      odds))))
        (t
          (lon*-filter-co (car lon)
                          test?
                          #'(lambda (n1 e1 o1)
                              (lon*-filter-co (cdr lon)
                                              test?
                                              #'(lambda (n2 e2 o2)
                                                  (funcall co
                                                           (cons n1 n2)
                                                           (* e1 e2)
                                                           (+ o1 o2)))))))))

(lon*-filter-co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) #'oddp #'friendo)
(defun evens-only*&co (lon) (lon*-filter-co lon #'oddp #'friendo))
(evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2))
