(ql:quickload :uiop)
(ql:quickload "cl-ppcre")
(load "../util.lisp")

(defstruct seat 
  min-row
  max-row
  min-column
  max-column)

(defun main ()
  (loop for line in (uiop:read-file-lines "./input.txt")
        maximize (decode line)))

(defun decode (string)
  (answer (offset-answer (seat-search string))))

(defun answer (seat)
  "Perform the sum"
  (+ (* 8 (seat-min-row seat)) (seat-min-column seat)))

(defun offset-answer (seat)
  "Decoding works better with rounded numbers, offset these again for the answer"
  ;; TODO protect against 0 row and column
  (decf (seat-min-row seat) 1)
  (decf (seat-min-column seat) 1)
  (decf (seat-max-row seat) 1)
  (decf (seat-max-column seat) 1)
  seat)

(defun seat-search (string)
  (reduce (lambda (seat value)
            (let* ((row-diff (- (seat-max-row seat) (seat-min-row seat)))
                   (row-offset (/ (+ 1 row-diff) 2))
                   (column-diff (- (seat-max-column seat) (seat-min-column seat)))
                   (column-offset (/ (+ 1 column-diff) 2)))
              (cond
                ((string= "F" value)
                 (decf (seat-max-row seat) row-offset)
                 seat)
                ((string= "B" value)
                 (incf (seat-min-row seat) row-offset)
                 seat)
                ((string= "L" value)
                 (decf (seat-max-column seat) column-offset)
                 seat)
                ((string= "R" value)
                 (incf (seat-min-column seat) column-offset)
                 seat)
                (t nil))))
          string
          :initial-value (make-seat-data)))

(defun make-seat-data ()
  (make-seat :min-row 1
             :max-row 128
             :min-column 1
             :max-column 8))

