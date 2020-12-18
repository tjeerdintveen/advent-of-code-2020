(ql:quickload :uiop)

(defun main ()
  (block nested-loops
    (let ((data (uiop:read-file-lines "input.txt")))
    ;; (let ((data (uiop:read-file-lines "sample.txt")))
      (dolist (lhs data)
        (dolist (rhs (subseq data 1))
          (let ((lhs-int (parse-integer lhs))
                (rhs-int (parse-integer rhs)))
            (when (eq 2020 (+ lhs-int rhs-int))
              (return-from nested-loops (* lhs-int rhs-int)))))))))
