(defpackage :5b
  (:use :common-lisp))

(ql:quickload :uiop)
(load "../util.lisp")

;; Answer is 610
(defun main ()
  (let* ((seat-ids (loop for line in (uiop:read-file-lines "./input.txt")
                         when (not (string= "" line))
                         collect (code->seat-id line)))
         (low (reduce #'min seat-ids)))

    (do ((nr low (+ 1 nr)))
        ((not (member nr seat-ids)) nr))))

(defun code->seat-id (code)
  (+
   (* 8 (string-binary-number
         (subseq code 0 7) #\B))
   (string-binary-number
    (subseq code 7) #\R)))

