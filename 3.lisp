;; 3.
(defun atom? (a)
  (not (listp a)))

;; ***** REMBER:
(defun rember (a lat)
  (cond
    ((null lat) '())
    ((eq (car lat) a)
     (cdr lat))
    (t
      (cons (car lat)
            (rember a (cdr lat))))))

(rember 'a nil)
  ;; => nil
(rember 'a '(a c d a c))
  ;; => (c d a c)

;; ***** FIRSTS:
(defun firsts (lol)
  (if (null lol)
    nil
    (cons (car (car lol))
          (firsts (cdr lol)))))

(firsts nil)
  ;; => ()
(firsts '((a b c) (d e f) (g h i)))
  ;; => (a d g)

;; ***** INSERTR:
(defun insertR (new old lat)
  (cond ((null lat) '())
        ((eq (car lat) old)
         (cons old (cons new (cdr lat))))
        (t
          (cons (car lat)
                (insertR new old (cdr lat))))))

(insertR 'topping 'fudge '(ice cream with fudge for dessert))
  ;; => (ice cream with fudge topping for dessert)
(insertR 'topping 'fudge nil)
  ;; => nil

;; ***** INSERTL:
(defun insertL (new old lat)
  (cond ((null lat) '())
        ((eq (car lat) old)
         (cons new lat))
        (t
          (cons (car lat)
                (insertL new old (cdr lat))))))

(insertL 'topping 'fudge '(ice cream with fudge for dessert))
  ;; => (ice cream with topping fudge for dessert)
(insertL 'topping 'fudge nil)
  ;; => nil

;; ***** SUBST:
(defun my-subst (new old lat)
  (cond ((null lat) '())
        ((eq (car lat) old)
         (cons new (cdr lat)))
        (t
          (cons (car lat)
                (my-subst new old (cdr lat))))))

(my-subst 'c 'x '(a x d c))
  ;; => (a c d c)
(my-subst 'c 'x nil)
  ;;; => ()

;; ***** SUBST2:
(defun subst2 (new o1 o2 lat)
  (cond ((null lat) '())
        ((or (eq (car lat) o1)
             (eq (car lat) o2))
         (cons new (cdr lat)))
        (t
          (cons (car lat)
                (subst2 new o1 o2 (cdr lat))))))

(subst2 'c 'x 'z '(a x d c))
  ;; => (a c d c)
(subst2 'c 'x 'z '(a z d c))
  ;; => (a c d c)

;; ***** MULTIREMBER:
(defun multirember (k lat)
  (cond ((null lat) '())
        ((eq (car lat) k)
         (multirember k (cdr lat)))
        (t
          (cons (car lat)
                (multirember k (cdr lat))))))

(multirember 'x '(a x c x d x c x))
  ;; => (a c d c)

;; ***** MULTIINSERTR:
(defun multiinsertR (new old lat)
  (cond ((null lat) '())
        ((eq (car lat) old)
         (cons old (cons new
                         (multiinsertR new old (cdr lat)))))
        (t
          (cons (car lat)
                (multiinsertR new old (cdr lat))))))

(multiinsertR 'x 'a '(a b c a a))
  ;; => (a x c a x a x)

;; ***** MULTIINSERTL:
(defun multiinsertL (new old lat)
  (cond ((null lat) '())
        ((eq (car lat) old)
         (cons new (cons old
                         (multiinsertL new old (cdr lat)))))
        (t
          (cons (car lat)
                (multiinsertL new old (cdr lat))))))

(multiinsertL 'x 'a '(a b c a a))
  ;; => (x a b c x a x a)


;; ***** MULTISUBST:
(defun multisubst (new old lat)
  (cond ((null lat) '())
        ((eq (car lat) old)
         (cons new
               (multisubst new old (cdr lat))))
        (t
          (cons (car lat)
                (multisubst new old (cdr lat))))))

(multisubst 'c 'x '(a x d x))
