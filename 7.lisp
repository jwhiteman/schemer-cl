;; 7
(defun member? (a lat)
  (cond ((null lat) nil)
        ((eq (car lat) a) t)
        (t
          (member? a (cdr lat)))))

(defun rember (a lat)
  (cond ((null lat) nil)
        ((eq (car lat) a)
         (rember a (cdr lat)))
        (t
          (cons (car lat)
                (rember a (cdr lat))))))

;; (set? lat)
(defun set? (lat)
  (if (null lat)
    t
    (and (not (member? (car lat) (cdr lat)))
         (set? (cdr lat)))))

;; (makeset lat)
(defun makeset (lat)
  (if (null lat)
    nil
    (let* ((head (car lat))
           (tail (cdr lat))
           (cdr-as-set (rember head tail)))
      (cons head (makeset cdr-as-set)))))

(makeset '(apple peach pear peach plum apple lemon peach))
;; => (APPLE PEACH PEAR PLUM LEMON)


;; (subset? s1 s2)
(defun subset? (s1 s2)
  (if (null s1)
    t
    (and (member? (car s1) s2)
         (subset? (cdr s1) s2))))

;; eqset?
(defun eqset? (s1 s2)
  (and (subset? s1 s2)
       (subset? s2 s1)))

;; intersect?
(defun intersect? (s1 s2)
  (if (null s1)
    nil ;; !
    (or (member? (car s1) s2)
        (intersect? (cdr s1) s2))))

(intersect? '(stewed tomatoes and macaroni) '(macaroni and cheese))

;; intersect
(defun intersect (s1 s2)
  (cond ((null s1) nil)
        ((member? (car s1) s2)
         (cons (car s1)
               (intersect (cdr s1) s2)))
        (t
          (intersect (cdr s1) s2))))

(intersect '(stewed tomatoes and macaroni) '(macaroni and cheese))

;; union
(defun my-union (s1 s2)
  (cond ((null s1) s2)
        ((member? (car s1) s2)
         (my-union (cdr s1) s2))
        (t
          (cons (car s1)
                (my-union (cdr s1) s2)))))

(my-union '(stewed tomatoes and macaroni casserole) '(macaroni and cheese))

;; intersectall
;; first pass, using fold
(defun fold (acc l f)
  (if (null l)
    acc
    (fold (funcall f acc (car l))
          (cdr l)
          f)))

(defun intersectall (l-set)
  (fold (car l-set)
        (cdr l-set) #'intersect))

(intersectall '((a b c) (c a d e) (e f g h a b)))

;; second pass, guessing how the book has it
(defun intersectall (l-set)
  (intersectall-helper (car l-set) (cdr l-set)))

(defun intersectall-helper (acc l-set)
  (if (null l-set)
    acc
    (intersectall-helper (intersect acc (car l-set))
                         (cdr l-set))))

;; dang: how the book actually has it: brilliant:
(defun intersectall (l-set)
  (if (null (cdr l-set))
    (car l-set)
    (intersect (car l-set)
               (intersectall (cdr l-set)))))

;; a-pair?
(defun a-pair? (x)
  (null (cdr (cdr x))))

;; firsts (again)
(defun firsts (los)
  (if (null los)
    '()
    (cons (car (car los))
          (firsts (cdr los)))))

(firsts '((a b c) (d e f) (g h i)))

(defun member? (a l)
  (if (null l)
    nil
    (or (eq (car l) a)
        (member? a (cdr l)))))

(member? 'c '(a c d c))
(member? 'c '(a x d x))

(defun set? (l)
  (cond ((null l) t)
        ((member? (car l) (cdr l)) nil)
        (t
          (set? (cdr l)))))

(set? '(a b c))
(set? '(a b c a))

;; fun?
(defun fun? (rel)
  (set? (firsts rel)))

(fun? '((8 3) (4 2) (7 6) (6 2) (3 4)))
(fun? '((d 4) (b 0) (b 9) (e 5) (g 4)))

;; revrel
(defun x-head (rel)
  (car (car rel)))
(defun y-head (rel)
  (car (cdr (car rel))))


(defun revrel (rel)
  (if (null rel)
    '()
    (cons (cons (y-head rel)
                (cons (x-head rel) nil))
          (revrel (cdr rel)))))

(revrel '((8 a) (pumpkin pie) (got sick)))

;; fullfun?
(defun fullfun? (fun)
  (and (fun? fun)
       (fun? (revrel fun))))

(fullfun? '((grape rasin) (plum prune) (stewed prune)))
(fullfun? '((grape rasin) (plum prune) (stewed grape)))
