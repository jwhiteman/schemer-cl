;; map - normal
(defun map-1 (map-f l)
  (if (null l)
    '()
    (cons (funcall map-f (car l))
          (map-1 map-f (cdr l)))))


;; map w/ leterc
(defun map-2 (map-f l)
  (labels ((_ (l)
              (if (null l)
                '()
                (cons (funcall map-f (car l))
                      (_ (cdr l))))))
    (_ l)))

(defun identity (x) x)

;; map w/ col
(defun map-3 (map-f l col)
  (if (null l)
    (funcall col '())
    (map-3 map-f
           (cdr l)
           #'(lambda (newlist)
               (funcall col
                        (cons (funcall map-f (car l))
                              newlist))))))

;; map w/ letrec & col
(defun map-4 (map-f l col)
  (labels ((_ (l col)
              (if (null l)
                (funcall col '())
                (_ (cdr l)
                   #'(lambda (newlist)
                       (funcall col
                                (cons (funcall map-f (car l))
                                      newlist)))))))
    (_ l col)))

;; map*
(defun atom? (l) (not (listp l)))
(defun map* (map-f lol)
  (cond ((null lol) '())
        ((atom? (car lol))
         (cons (funcall map-f (car lol))
               (map* map-f (cdr lol))))
        (t
          (cons (map* map-f (car lol))
                (map* map-f (cdr lol))))))

(map* #'1+ '(1 2 (3) ((4 (5) nil))))

;; map* w/ letrec
(defun map*-2 (map-f lol)
  (labels ((_ (lol)
              (cond ((null lol) '())
                    ((atom? (car lol))
                     (cons (funcall map-f (car lol))
                           (_ (cdr lol))))
                    (t
                      (cons (_ (car lol))
                            (_ (cdr lol)))))))
    (_ lol)))

(map*-2 #'1+ '(1 2 (3) ((4 (5) nil))))

;; map* w/ col
(defun map*co (map-f lol co)
  (cond ((null lol) (funcall co '()))
        ((atom? (car lol))
         (map*co map-f
                 (cdr lol)
                 #'(lambda (newlist)
                     (funcall co
                              (cons (funcall map-f (car lol))
                              newlist)))))
        (t
          (map*co map-f
                  (car lol)
                  #'(lambda (nl1)
                      (map*co map-f
                              (cdr lol)
                              #'(lambda (nl2)
                                  (funcall co (cons nl1 nl2)))))))))

(map*co #'1+ '(1 2 (3) ((4 (5) nil))) #'identity)
(map*co #'1+ '(1 2 3 4 5) #'identity)

;; map* w/ col & letrec
(defun map*co-2 (map-f lol co)
  (labels ((_ (lol co)
              (cond ((null lol) (funcall co '()))
                    ((atom? (car lol))
                     (_ (cdr lol)
                        #'(lambda (newlist)
                            (funcall co
                                     (cons (funcall map-f (car lol))
                                           newlist)))))
                    (t
                      (_ (car lol)
                         #'(lambda (nl1)
                             (_ (cdr lol)
                                #'(lambda (nl2)
                                    (funcall co
                                             (cons nl1 nl2))))))))))
    (_ lol co)))

(map*co-2 #'1+ '(1 2 3 4 5) #'identity)
(map*co-2 #'1+ '(1 2 (3) ((4 (5) nil))) #'identity)
