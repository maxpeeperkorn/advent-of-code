(require :advent-of-code)

(defparameter *input-file* "data/input_08.txt")


(defun make-grid (lines)
  (mapcar #'(lambda (line)
              (mapcar #'(lambda (x)
                          (parse-integer (concatenate 'string (list x))))
                      (coerce line 'list)))
          lines))


(defun check-visibility (tree-line)
  (let ((max -1))
    (mapcar #'(lambda (tree)
                (if (> tree max)
                    (and (setf max tree) 1)
                    0))
            tree-line)))


(defun check-sight (tree-line)
  (mapcar #'(lambda (tree-line-chunk)
              (if (> (length tree-line-chunk) 1)
                  (let ((the-tree (first tree-line-chunk))
                        (the-rest (rest tree-line-chunk)))
                    (if (< (first the-rest) the-tree)
                        (let ((max-tree -1))
                          (reduce #'(lambda (acc tree)
                                      (+ acc
                                         (if (< max-tree the-tree)
                                             (if (> tree max-tree)
                                                 (and (setf max-tree tree) 1)
                                                 1)
                                             0)))
                                  the-rest :initial-value 0))
                        1))
                  0))
          (mapcar #'reverse (advent-of-code:chunk-list tree-line))))


(defun traverse-the-forest (grid reduce-fn score-fn traversal-fn)
  (let ((grid-transpose (advent-of-code:transpose grid)))
    (reduce reduce-fn
      (apply #'append
             (mapcar #'(lambda (left up right down)
                         (mapcar score-fn left right up down))
                     ;; left
                     (advent-of-code:->> grid
                                         (mapcar traversal-fn))
                     ;; up
                     (advent-of-code:->> grid-transpose
                                         (mapcar traversal-fn)
                                         advent-of-code:transpose)
                     ;; right
                     (advent-of-code:->> grid
                                         (mapcar #'reverse)
                                         (mapcar traversal-fn)
                                         (mapcar #'reverse))
                     ;; down
                     (advent-of-code:->> grid-transpose
                                         (mapcar #'reverse)
                                         (mapcar traversal-fn)
                                         (mapcar #'reverse)
                                         advent-of-code:transpose))))))


(defun score-visibility (left right up down)
  (if (> (+ left right up down) 0)
      1
      0))


(defun score-sight (left right up down)
  (* left right up down))


(defun main ()
  (with-open-file (stream *input-file*)
    (let ((grid (make-grid (advent-of-code:read-lines stream))))
      (list "A:" (traverse-the-forest grid
                                     #'+
                                     #'score-visibility
                                     #'check-visibility)
            "B:" (traverse-the-forest grid
                                     #'max
                                     #'score-sight
                                     #'check-sight)))))


(format t "~%~A -> ~A" "Day 8" (main))
