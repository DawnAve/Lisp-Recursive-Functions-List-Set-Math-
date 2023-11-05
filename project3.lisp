;List Functions
(defun .contains (L X)
  (if (null L)
      NIL
      (if (equalp (car L) X)
          T
          (.contains (cdr L) X))))

(defun .element-at (L I)
  ; only two parameters, so call another function with an addition parameter
  (if  (>= I 0) 
      (.element-at-iter L I 0)))

(defun .element-at-iter (L I current-index)   ;helper function
  (cond
    ((null L) NIL)
    ((= current-index I) (car L))
    (t (.element-at-iter (cdr L) I (+ current-index 1)))))

(defun .addtoend (X L)
    (if (null L)
        (list X)
        (cons (car L) (.addtoend X (cdr L)))))

(defun .ntimes (F X N)
  (if (= N 0)
      X
      (.ntimes F (funcall F X) (- N 1))))

;Set Functions
(defun .element-of (X S)
    (if (.contains S X)
        T
        NIL))

(defun .insert (S X)
    (if (.element-of X S)
        S
        (cons X S)))

(defun .union (S1 S2)
  (cond
    ((null S1) S2)
    ((null S2) S1)
    ((.element-of (car S1) S2) (.union (cdr S1) S2)) ;if the first element in S1 is in S2, do recursion to the rest
    (t (cons (car S1) (.union (cdr S1) S2))))) ;if not, add to S2, do recursion to the rest

(defun .superseteq (S1 S2)
    (cond
        ((null S2) T)
        ((.element-of (car S2) S1)(.superseteq(cdr S2) S1))
        NIL))

;Math Funtioins
(defun .pow (X Y)
  (if (= Y 0)
      1
      (if (< Y 0)
          (/ 1.0 (.pow X (- Y)))
          (* X (.pow X (- Y 1))))))

(defun .log2int (x)
    (if (< x 2)
        0
        (+ 1 (.log2int (/ x 2)))))

(defun .nth-tri (N)
    (if (= N 1)
        1
        (+ N (.nth-tri (- N 1)))))

(defun .monthly-payment (P R N)
    (/ (* R P) (- 1 (/ 1 (.pow(+ 1 R) N)))))

