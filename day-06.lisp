(require :advent-of-code)

(defparameter *input-file* "data/input_06.txt")


(defun unique-only (set)
  (remove-duplicates (coerce set 'list)))


(defun find-marker (str len i)
  (let ((marker (unique-only (subseq str (- i len) i))))
    (if (= (length marker) len)
        i
        (find-marker str len (+ i 1)))))


(defun day-six ()
  (with-open-file (stream *input-file*)
    (let ((line (car (advent-of-code:read-lines stream))))
      (list "A" (find-marker line 4 4)
            "B" (find-marker line 14 14)))))

(day-six)
