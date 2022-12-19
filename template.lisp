(require :advent-of-code)

(defparameter *input-file* "data/input_xx.txt")

(defun day-xxxx ()
  (with-open-file (stream *input-file*)
    (let ((lines (advent-of-code:read-lines stream)))
      nil)))

(day-xxxx)
