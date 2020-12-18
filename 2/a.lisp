(defpackage :2a
  (:use :common-lisp))

(in-package :2a)

(ql:quickload :uiop)

(defun main ()
  (let ((data (uiop:read-file-lines "./input.txt"))
  ;; (let ((data (uiop:read-file-lines "./sample.txt"))
        (count 0))
      (dolist (line data)
        (if (is-valid line)
            (incf count)))
    count))

(defun is-valid (line)
  (let ((char (target-char line))
        (str (target-string line)))

    (and
     (>= (char-count char str)
          (minimum line))
     (<= (char-count char str)
         (maximum line)))))

(defun char-count (target-char string)
  (let ((count 0))
    (map 'string
         #'(lambda (char)
             (if (string= char target-char)
                 (incf count))
             char)
         string)
    count))

(defun minimum (line)
  (parse-integer (string-until #\- line)))

(defun maximum (line)
  (parse-integer (string-until
                  #\Space
                  (string-after #\- line))))

(defun target-char (string)
  (let ((pos (position #\Space string)))
    (subseq string (+ 1 pos) (+ 2 pos))))

(defun target-string (line)
  (string-after #\Space (string-after #\: line)))

(defun string-after (char string)
  (subseq string (+ (position char string) 1)))

(defun string-until (char string)
  (subseq string 0 (position char string)))
