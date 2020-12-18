(defpackage :7
  (:use :common-lisp))

(ql:quickload :uiop)
(load "../util.lisp")

;; 300 for real answer, 4 for sample.txt
(defun solve-a (&optional (use-sample nil))
  (setq *visited* '())
  (walk-graph "shiny gold"
              (hash-from-data (if use-sample
                                  "sample.txt"
                                  "input.txt")))
  (length *visited*))

;;8030 for real answer, 126 for sample.txt
(defun solve-b (&optional (use-sample nil))
  (setq *visited* '())
                                        ; Deduce one, we don't want to count the first
  (- (walk-graph-count "shiny gold"
                       (hash-from-data2 (if use-sample
                                            "sample.txt"
                                            "input.txt")))
     1))


(defvar *visited* '())

(defun hash-from-data (file)
  "Make an owned by list where the right side of the input becomes the key. Amount is added to the left side of the input."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (line (uiop:read-file-lines file))
      (dolist (bag (rhs-bags line))
        (hash-append-list table (nth 1 bag) (list (first bag) (lhs-bag line)))))
    table))

(defun hash-from-data2 (file)
  "List as is presented, keys are keys, values are values"
  (let ((table (make-hash-table :test 'equal)))
    (dolist (line (uiop:read-file-lines file))
      (dolist (bag (rhs-bags line))
        (hash-append-list table (lhs-bag line) (list (first bag) (nth 1 bag)))))
    table))

(defun walk-graph (bag hash)
  (let ((values (gethash bag hash '())))
    (dolist (value values)
      (let ((key-bag (nth 1 value)))
        (when (not (member key-bag *visited* :test #'string=))
          (push key-bag *visited*)
          (walk-graph key-bag hash))))))

(defun walk-graph-count (bag hash)
  (let ((values (gethash bag hash '()))
        (count 1))
    (dolist (value values)
      (let ((key-bag (nth 1 value))
            (bag-count (first value)))
        ;; As opposed to the first challenge, we do revisit bags that have been visited before
        (push key-bag *visited*)
        (incf count (* bag-count
                       (walk-graph-count key-bag hash)))))
    count))

;; Parsing

(defun lhs-bag (string)
  (string-before string " bag"))

(defun rhs-bags (string)
  (let ((remainder (string-after string "contain ")))
    (if (string= "no other bags." remainder)
        nil
        (parse-rhs-remainder remainder))))

(defun parse-rhs-remainder (remainder)
  (mapcar (lambda (bag-string)
            (parse-bag (trim-whitespace bag-string)))
          (uiop:split-string remainder :separator ",")))

(defun parse-bag (string)
  "Parses a bag with a prefix number"
  (let ((str (string-before string " bag")))
    (list
     (parse-integer (subseq str 0 1))
     (subseq str 2))))
