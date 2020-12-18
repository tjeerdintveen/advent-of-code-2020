(defpackage :3a
  (:use :common-lisp))
(in-package :3a)
(ql:quickload :uiop)

(defun main ()
  (let ((data (uiop:read-file-lines "./sample.txt"))
        (index 0)
        (count 0))
    (dolist (line data)
      (if (has-tree line (mod index (length line)))
          (incf count))
      (incf index 3))
    count))

(defun has-tree (line index)
  (eq
   (elt line index)
   #\#))
