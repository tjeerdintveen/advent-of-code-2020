(defpackage :3b
  (:use :common-lisp))
(in-package :3b)
(ql:quickload :uiop)

;; 4355551200
(defun main ()
  ;; (let ((data (uiop:read-file-lines "./sample.txt")))
  (let ((data (uiop:read-file-lines "./input.txt")))

    (*
     (count-trees data 1 1)
     (count-trees data 3 1)
     (count-trees data 5 1)
     (count-trees data 7 1)
     (count-trees data 1 2)
     )))

(defun count-trees (data right down)
  (let ((count 0))
    (do ((row 0 (+ row down))
          (column 0 (+ column right)))
         ((>= row (length data)) count)
      (let ((line (elt data row)))
        (if (has-tree line (mod column (length line)))
            (incf count))))))


(defun has-tree (line index)
  (eq
   (elt line index)
   #\#))

