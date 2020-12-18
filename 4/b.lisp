(ql:quickload :uiop)
(ql:quickload "cl-ppcre")
(load "../util.lisp")

(defun main ()
  (let ((valid-passports 0))
    (read-lines "./input.txt" "" (lambda (line)
                                   (if (is-valid (line->passport line))
                                       (incf valid-passports))))
    valid-passports))

(defun is-valid (passport)
  (and
   (hash-apply-if-key passport "hcl" #'validate-color)
   (hash-apply-if-key passport "pid" #'validate-passport-id)
   (hash-apply-if-key passport "hgt" #'validate-height)
   (hash-apply-if-key passport "ecl" #'validate-eye-color)
   (hash-apply-if-key passport "byr" (lambda (value)
                                       (validate-min-max 1920 2002 (parse-integer value))))
   (hash-apply-if-key passport "iyr" (lambda (value)
                                       (validate-min-max 2010 2020 (parse-integer value))))
   (hash-apply-if-key passport "eyr" (lambda (value)
                                       (validate-min-max 2020 2030 (parse-integer value))))))

(defun validate-eye-color (color)
  (find color '("amb" "blu" "brn" "gry" "grn" "hzl" "oth") :test #'string=))

(defun validate-passport-id (passport-id)
  (equal 9 (count-if #'digit-char-p passport-id)))

(defun validate-height (string)
  (let ((nr (parse-integer (take-last 2 string))))
    (or
     (and (ends-with "cm" string) (validate-min-max 150 193 nr))
     (and (ends-with "in" string) (validate-min-max 59 76 nr)))))

(defun validate-color (value)
  (ppcre:scan "^#(?:[0-9a-fA-F]{3}){1,2}$" value))

(defun validate-min-max (min max value)
  (and
   (<= min value)
   (>= max value)))

(defun line->passport (line)
  "Turn a line in a passport hash table \"a:b\" \":c:d\" etc"
  (hash-table-from-pairs
   (mapcar (lambda (string)
             (uiop:split-string string :separator ":"))
           (uiop:split-string line))))
