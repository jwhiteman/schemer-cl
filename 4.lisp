;; 4.
;; (+ n m)
(defun o+ (n m)
  (if (= n 0)
    m
    (1+ (o+ (1- n) m))))

;; (- n m)
(defun o- (n m)
  (if (= m 0)
    n
    (1- (o- n (1- m)))))

;; (addtup tup)
(defun addtup (tup)
  (if (null tup)
    0
    (+ (car tup)
       (addtup (cdr tup)))))

;; (* n m)
(defun o* (n m)
  (if (= 0 m)
    0
    (+ n (o* n (1- m)))))

;; (tup+ tup1 tup2)
(defun tup+ (tup1 tup2)
  (if (null tup2)
    '()
    (cons (+ (car tup1) (car tup2))
          (tup+ (cdr tup1) (cdr tup2)))))

(defun tup+i (tup1 tup2)
  (cond ((null tup1) tup2)
        ((null tup2) tup1)
        (t
          (cons (+ (car tup1) (car tup2))
                (tup+i (cdr tup1) (cdr tup2))))))

;; (> n m)
(defun o> (n m)
  (cond ((= 0 n) nil)
        ((= 0 m) t)
        (t
          (o> (1- n)
              (1- m)))))

;; (< n m)
(defun o< (n m)
  (cond ((= m 0) nil)
        ((= n 0) t)
        (t
          (o< (1- n) (1- m)))))

;; (= n m)
(defun o= (n m)
  (cond ((= 0 n m) t)
        ((or (= 0 n) (= 0 m)) nil)
        (t
          (o= (1- n) (1- m)))))

;; (** n m)
(defun o** (n m)
  (cond ((= 0 m) 1)
        ((= 1 m) n)
        (t
          (* n
             (o** n (1- m))))))

;; (/ n m)
(defun o/ (n m)
  (if (< n m)
    0
    (1+ (o/ (- n m) m))))

;; (length lat)
(defun my-length (lat)
  (if (null lat)
    0
    (1+ (my-length (cdr lat)))))

;; (pick n lat)
(defun pick (n lat)
  (if (= n 1)
    (car lat)
    (pick (1- n)
          (cdr lat))))

;; (rempick n lat)
(defun rempick (n lat)
  (if (= n 1)
    (cdr lat)
    (cons (car lat)
          (rempick (1- n) (cdr lat)))))

;; (no-nums lat)
(defun no-nums (lat)
  (cond ((null lat) nil)
        ((numberp (car lat))
                  (no-nums (cdr lat)))
        (t
          (cons (car lat)
                (no-nums (cdr lat))))))

;; (all-nums lat)
(defun all-nums (lat)
  (cond ((null lat) nil)
        ((numberp (car lat))
         (cons (car lat)
               (all-nums (cdr lat))))
        (t
          (all-nums (cdr lat)))))

;; (equan a1 a2)
(defun equan (a1 a2)
  (cond ((and (numberp a1) (numberp a2))
         (= a1 a2))
        ((or (numberp a1) (numberp a2))
         nil)
        (t
          (eq a1 a2))))

;; (occur a lat)
(defun occur (a lat)
  (cond ((null lat) 0)
        ((equan a (car lat))
         (1+ (occur a (cdr lat))))
        (t
          (occur a (cdr lat)))))

;; (one? n)
(defun one? (n) (= 1 n))
