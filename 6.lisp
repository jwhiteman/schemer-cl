;; 6

;; helpers
(defun atom? (a) (not (listp a)))

(defun left (nexp) (car nexp))
(defun right (nexp) (car (cdr (cdr nexp))))
(defun op (nexp) (car (cdr nexp)))

;; (numbered? e)
(defun numbered? (nexp)
  (if (atom? nexp)
    (numberp nexp)
    (and (numbered? (left nexp))
         (numbered? (right nexp)))))

;; (value nexp)
(defun value (nexp)
  (cond ((atom? nexp) nexp)
        ((eq (op nexp) '+)
         (+ (value (left nexp))
            (value (right nexp))))
        ((eq (op nexp) '-)
         (- (value (left nexp))
            (value (right nexp))))
        ((eq (op nexp) '*)
         (* (value (left nexp))
            (value (right nexp))))))

;; sero, edd1, zub1, sadd
(defun sero? (l) (null l))
(defun edd1 (l) (cons nil l))
(defun zub1 (l) (cdr l))
(defun sadd (n m)
  (if (sero? m)
    n
    (edd1 (sadd n (zub1 m)))))
