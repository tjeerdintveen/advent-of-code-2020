(ql:quickload :uiop)
(load "../util.lisp")

;; Sample gives 5, answer is 1553
;; Takes 193 loops to get there
(defun solve-a (&optional (use-sample nil))
  (run (tokenize use-sample) 0))

(defun solve-b (&optional (use-sample nil))
  (let ((found nil))
    (loop for program in (program-variations (tokenize use-sample))
          while (not found) do
            (let ((value (run2 program)))
              (if value
                  (setf found value))))
    found))

(defun program-variations (program)
  "Return programs with variations"
  (let ((programs '())
        (index 0))
    (dolist (instruction program)
      (let ((current-program (cl:copy-tree program))
            (op (first instruction))
            (nr (second instruction)))

        (cond
          ((string= "nop" op)
           (setf (nth index current-program) (list "jmp" nr)))
          ((string= "jmp" op)
           (setf (nth index current-program) (list "nop" nr)))
          (t nil))

        (push current-program programs)
        (incf index)))
    programs))

(defun tokenize (&optional (use-sample nil))
  (let ((program '()))
    (dolist (line (uiop:read-file-lines (if use-sample
                                            "sample.txt"
                                            "input.txt")))
      (let* ((instruction (uiop:split-string line))
             (op (first instruction))
             (nr (parse-integer (second instruction))))

        (push (list op nr) program)))
    (nreverse program)))

(defun run (program accumulator)
  (let ((index 0)
        (visited-indices '()))
    (loop while (not (member index visited-indices)) do
      (pushnew index visited-indices)
      (let* ((instruction (nth index program))
             (op (first instruction))
             (nr (second instruction)))

        (cond
          ((string= op "acc")
           (incf accumulator nr)
           (incf index))
          ((string= op "jmp")
           (incf index nr))
          (t (incf index))))))
  accumulator)

(defun run2 (program)
  (let ((index 0)
        (accumulator 0)
        (max-cycles 300)
        (loop-count 0) 
        (infinite-loop nil)) 

    (loop while (and (< index (length program))
                     (<= loop-count max-cycles)) do

                       (incf loop-count)

                       (if (eq max-cycles loop-count)
                           (setf infinite-loop t))

                       (let* ((instruction (nth index program))
                              (op (first instruction))
                              (nr (second instruction)))

                         (cond
                           ((string= op "acc")
                            (incf accumulator nr)
                            (incf index))
                           ((string= op "jmp")
                            (incf index nr))
                           (t (incf index)))))

    (if infinite-loop
        nil
        accumulator)))
