(defun split-by (fn list)
  "Find element to split, and splits list into smaller ones"
  (split fn list `() `()))

(defun split (fn list acc current)
  (cond
    ((and (null current) (null list))
     acc)
    ((null list)
     (cons current acc))
    ((funcall fn (car list))
     (split fn (cdr list)
            (cons current acc) `()))
    (t
     (split fn (cdr list) acc (cons (car list) current)))))

(defun flatten (l)
  (cond ((null l) nil)
        ((atom (car l)) (cons (car l) (flatten (cdr l))))
        (t (append (flatten (car l)) (flatten (cdr l))))))

(defun keys (list-of-pairs)
  "Get keys from list of pairs"
  (mapcar (lambda (pair)
            (car pair))
          list-of-pairs))

(defun char-to-int (val)
  "Convert character to integer code"
  (or
   (digit-char-p val) ; numbers need digit-char-p
   (char-int val))) ; alphabet needs char-int

(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

(defun hash-apply-if-key (hash-table key fn)
  (if (gethash key hash-table)
      (funcall fn (gethash key hash-table))))

(defun ends-with (sub string)
  (search sub string :from-end t))

(defun take-last (amount sequence) 
  (subseq sequence 0 (- (length sequence) amount)))

(defun read-lines (file end-of-line fn)
  "Allow for any char to be defined as end-of-line, not just newline"
  (let ((current-line nil))
    (dolist (line (uiop:read-file-lines file))
      (let ((is-eol (string= line end-of-line)))
        (when is-eol
          (if current-line
              (funcall fn current-line))
          (setf current-line nil))
        
        (if (not is-eol)
          (if current-line
              (setf current-line (concatenate `string current-line " " line))
              (setf current-line line)))))
    ;; TODO Make sure last line isn't repeated (empty?)
    (funcall fn current-line)))

(defun hash-table-from-pairs (pairs)
  "List of pairs turn into a hash, uses equal (string) matching"
  (reduce (lambda (hash pair)
            (setf (gethash (first pair) hash) (second pair))
            hash)
          pairs
          :initial-value (make-hash-table :test 'equal)))

(defun print-hash (hash)
  (maphash (lambda (k v)
             (format t "key: ~a value: ~a ~%" k v))
           hash))

(defun hash-values (table)
  (loop for v being each hash-value of table
        collect v))

(defun string->set (string)
  (reduce (lambda (acc char)
            (pushnew char acc))
          string
          :initial-value '()))

(defun string-binary-number (string char)
  "Turns a string like \"FBFBBFF\" to \"10110011\" to number 44, where #\B is the char. If the char in the string is found, it turns to 1, otherwise a 0"
  (parse-integer (string->binary string char) :radix 2))

(defun string->binary (string char)
  "Turns a string like \"FBFBBFF\" to \"10110011\" where #\B is the char. If the char in the string is found, it turns to 1, otherwise a 0"
  (map `string (lambda (current-char) (if (char= char current-char) #\1 #\0)) string))

(defun string-before (string sub-string)
  "Returns string before target string"
  (let ((pos (search sub-string string)))
    (if pos
        (subseq string 0 pos)
        nil)))

(defun string-after (string sub-string)
  "Returns string after target string"
  (let ((pos (search sub-string string)))
    (if pos
        (subseq string (+ (length sub-string) pos))
        nil)))

(defun push-last (value list)
  (push value (cdr (last list))))

(defun hash-append-list (table key added-value)
  "Adds value to a list in a hash' key."
  (let ((list (gethash key table '())))
    (setf
     (gethash key table '())
     (push
      added-value
      list))))

(defun trim-whitespace (string)
  "Only spaces for now"
  (string-trim " " string))

(defun make-window (list start size)
  "Slice a list from beginning to end, keeps end offset in mind"

  (cond
    ((>= start (length list))
     '())
    ((> (+ start size) (length list))
     (subseq list start (length list)))
    (t 
     (subseq list start (+ start size)))))
