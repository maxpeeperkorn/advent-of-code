(require :advent-of-code)

(defparameter *input-file* "data/input_03.txt")

(defparameter *alphabet* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")


(defun get-letter-value (letter)
  (+ (position letter *alphabet*) 1))


(defun split-in-half (str)
  (let ((str-len (length str)))
    (if (evenp str-len)
        (let ((i (/ str-len 2)))
          (values (subseq str 0 i) (subseq str i)))
        (error "String length is not even. Length is ~D" str-len))))


(defun make-unique-set (str)
  (remove-duplicates (coerce str 'list)))


(defun find-priorities (rucksack)
  (multiple-value-bind (left right) (split-in-half rucksack)
    (reduce #'(lambda (acc letter)
                (+ acc (get-letter-value letter)))
            (intersection (make-unique-set left)
                          (make-unique-set right))
            :initial-value 0)))


(defun find-badges (chunk)
  (multiple-value-bind (a b c) (values-list (mapcar #'make-unique-set chunk))
    (get-letter-value (car (intersection (intersection a b) c)))))


(defun calc-score (fn list)
  (reduce #'+ (mapcar fn list)))


(defun day-three ()
  (with-open-file (stream *input-file*)
    (let ((lines (advent-of-code:read-lines stream)))
      (print (list "A" (calc-score #'find-priorities
                                   lines)
                   "B" (calc-score #'find-badges
                                   (advent-of-code:partition 3 lines)))))))

(day-three)
