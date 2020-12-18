(defpackage :6a
  (:use :common-lisp))

(ql:quickload :uiop)
(load "../util.lisp")

(defun main ()
  (let ((count 0))
    (read-lines "input.txt" "" (lambda (line)
                                  (let ((a-set `()))
                                    (map nil (lambda (char)
                                               (if (not (string= char " "))
                                                   (pushnew char a-set)))
                                         line)
                                    (incf count (length a-set)))))
    count))
