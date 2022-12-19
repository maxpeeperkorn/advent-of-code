(require :advent-of-code)

(defparameter *input-file* "data/input_01.txt")

(defun calc-score (lst)
  (sort
    (reduce #'(lambda (acc x)
                (if (= (length x) 0)
                  (cons 0 acc)
                  (cons (+ (car acc)
                           (parse-integer x))
                        (cdr acc))))
            lst :initial-value (list 0))
    #'>))

(defun day-one ()
  (with-open-file (stream *input-file*)
    (let ((results (calc-score (advent-of-code:read-lines stream))))
      (print (list "A" (car results)
                   "B" (reduce #'+ (subseq results 0 3)))))))

(day-one)
