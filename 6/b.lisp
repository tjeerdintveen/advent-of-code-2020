(defpackage :6b
  (:use :common-lisp))

(ql:quickload :uiop)
(load "../util.lisp")

; Solving it with sets
(defun main ()
  (let ((count 0))
    (read-lines "input.txt" "" (lambda (line)
                                  (let ((set nil)
                                        (is-new t)) ; An empty set can be valid mid-intersecting so we need to track if it's a fresh set.
                                    (dolist (person-answers (uiop:split-string line))
                                      (let ((answers (string->set person-answers)))
                                        (if is-new
                                            (and (setf set answers)
                                                 (setf is-new nil))
                                            (setf set (intersection set answers)))))
                                    (incf count (length set)))))
    count))

;; Or use a hash
;; (defun main ()
;;   (let ((count 0))
;;     (read-lines "input.txt" "" (lambda (line)
;;                                   (let ((hash (make-hash-table))
;;                                         (num-people 1))
;;                                     (map nil (lambda (char)
;;                                                (if (string= char " ")
;;                                                    (incf num-people)
;;                                                    (incf (gethash char hash 0))))
;;                                          line)

;;                                     (incf count
;;                                           (count-if
;;                                            (lambda (value) (eq value num-people))
;;                                            (hash-values hash))))))
;;     count))

