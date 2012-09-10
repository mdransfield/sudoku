;;;; sudoku.lisp

(in-package #:sudoku)

(defun decode (index)
  "Transform INDEX into sudoku array into row/column references"
  (multiple-value-list (floor index 9)))

(defun encode (row col)
  "Transform ROW COLumn references into sudoku array index."
  (+ col (* row 9)))

(defun row (sudoku index)
  "Values already taken in row given by INDEX in SUDOKU."
  (let ((r (first (decode index))))
    (coerce (remove-if #'zerop (subseq sudoku (encode r 0) (encode (1+ r) 0))) 'list)))

(defun col (sudoku index)
  "Values already taken in column given by INDEX in SUDOKU."
  (loop with c = (second (decode index))
	for r from 0 below 9
	for v = (aref sudoku (encode r c))
	unless (zerop v)
	  collect v))

(defun sqr (sudoku index)
  "Values already taken in the small square in SUDOKU that INDEX is in."
  (loop with coords = (decode index)
	with r = (* 3 (floor (first coords) 3))
	with c = (* 3 (floor (second coords) 3))
	for i from r below (+ 3 r)
	append (loop for j from c below (+ 3 c)
		     for v = (aref sudoku (encode i j))
		     unless (zerop v)
		       collect v)))

(defun possibilities (sudoku index)
  "Possible values for SUDOKU at INDEX."
  (set-difference (list 9 8 7 6 5 4 3 2 1)
		  (union (union (row sudoku index)
				(col sudoku index))
			 (sqr sudoku index))))

(defun iterate (sudoku)
  (loop for i from 0 below 81
	for p = (if (zerop (aref sudoku i)) (possibilities sudoku i) nil)
	when (= 1 (length p))
	  do (setf (aref sudoku i) (first p))
	     (return sudoku)
	finally (return nil)))

(defun sreduce (sudoku)
  (loop while (iter sudoku)))

(defun show (sudoku)
  (loop for r from 0 below 9
	do (format t "~&~s~%" (subseq sudoku (encode r 0) (encode (1+ r) 0)))))

(defun solvedp (sudoku)
  (not (position 0 sudoku)))

(defun solve (s)
  (sreduce s)
  (unless (solvedp s)
    ))

(defparameter *s*
  #(0 5 0 7 0 0 2 0 0
    0 0 0 0 8 5 0 0 0
    3 0 0 2 0 0 7 5 0
    0 8 9 0 0 7 0 0 4
    0 0 2 0 0 0 3 0 0
    7 0 0 5 0 0 9 6 0
    0 2 6 0 0 9 0 0 3
    0 0 0 6 4 0 0 0 0
    0 0 7 0 0 3 0 4 0))

(defparameter *s1*
  #(0 0 0 0 0 0 0 0 0
    0 0 4 0 5 0 6 0 0
    8 9 0 6 0 3 0 7 4
    0 3 0 0 0 0 0 6 0
    5 0 0 9 0 8 0 0 1
    0 2 0 0 0 0 0 9 0
    6 7 0 5 0 1 0 8 3
    0 0 1 0 9 0 2 0 0
    0 0 0 0 0 0 0 0 0))