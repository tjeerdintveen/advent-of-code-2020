(ql:quickload :uiop)
(load "../util.lisp")

(defparameter *expected-keys* `("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"))

(defun main ()
  (let ((data (uiop:read-file-lines "./input.txt"))
        ;; (let ((data (uiop:read-file-lines "./sample.txt"))
        (current-passport nil)
        (valid-passports 0))

    (dolist (line data)
      (if (string= "" line)
          (progn
              (if (is-valid current-passport)
                  (incf valid-passports))
             (setq current-passport nil))

          (dolist (pair (pairs line))
            (setq current-passport (cons pair current-passport)))))

    (if (is-valid current-passport)
        (incf valid-passports))
    valid-passports))

(defun is-valid (passport)
  (let ((count (length
                (intersection *expected-keys*
                              (keys passport) :test #'string=))))
    (or (eq 7 count)
        (eq 8 count))))

(defun validate-height (string)
  (let ((nr (parse-integer (subseq string 0 (- (length string) 2)))))
    (cond
      ((search "cm" string :from-end t)
       (validate-min-max 150 193 nr))
      ((search "in" string :from-end t)
       (validate-min-max 59 76 nr))
      (t nil))))

(defun validate-color (value)
  "Validates color #ffffff, pound with a-f, A-F or 0-9"
  (and
   (equal 7 (length value))
   (equal #\# (elt value 0))
   (every (lambda (val)
            (let ((int-value (char-to-int val))) 
              (or
               (validate-min-max 0 9 int-value)
               (validate-min-max 65 70 int-value) ; A-F
               (validate-min-max 97 102 int-value)))) ; a-f
          (subseq value 1))))

(defun validate-min-max (min max value)
  (and
   (<= min value)
   (>= max value)))

(defun pairs (string)
  "string is hcl:#ffffff hgt:74in for example, returns ((hcl #ffffff) (hgt: 74in))"
  (mapcar (lambda (string)
            (uiop:split-string string :separator ":"))
          (uiop:split-string string)))
