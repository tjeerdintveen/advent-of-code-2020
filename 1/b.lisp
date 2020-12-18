(defpackage :1b
   (:use :common-lisp))

(in-package :1b)

(ql:quickload :uiop)

(defun main ()
  (block nested-loops
    (let ((data (uiop:read-file-lines "input.txt")))
    ;; (let ((data (uiop:read-file-lines "sample.txt")))
      (dolist (first data)
        (dolist (second (subseq data 1))
          (dolist (third (subseq data 2))
            (let ((first-int (parse-integer first))
                  (second-int (parse-integer second))
                  (third-int (parse-integer third)))
              (when (eq 2020 (+ first-int second-int third-int))
                (format t "~a ~a ~a" first-int second-int third-int)
                (return-from nested-loops (* first-int second-int third-int))))))))))
