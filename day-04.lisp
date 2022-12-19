(require :advent-of-code)

(defparameter *input-file* "data/input_04.txt")


(defun both-subsetp (lista listb)
  (or (subsetp lista listb)
      (subsetp listb lista)))


(defun get-sections (sections)
  (multiple-value-bind (from to) (values-list
                                    (advent-of-code:split-string sections
                                                                 :delimiter #\-))
    (advent-of-code:range (parse-integer from) (+ (parse-integer to) 1))))


(defun find-subsets (pair)
  (multiple-value-bind (a b) (values-list
                               (advent-of-code:split-string pair
                                                            :delimiter #\,))
    (if (both-subsetp (get-sections a) (get-sections b))
        1
        0)))


(defun find-overlap (pair)
  (multiple-value-bind (a b) (values-list
                               (advent-of-code:split-string pair
                                                            :delimiter #\,))
    (if (> (length (intersection (get-sections a) (get-sections b))) 0)
        1
        0)))


(defun calc-score (fn lines)
  (reduce #'+ (mapcar fn lines)))


(defun day-four ()
  (with-open-file (stream *input-file*)
    (let ((lines (advent-of-code:read-lines stream)))
      (list "A" (calc-score #'find-subsets lines)
            "B" (calc-score #'find-overlap lines)))))

(day-four)
