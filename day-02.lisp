(require :advent-of-code)

(defparameter *input-file* "data/input_02.txt")

;; @todo refactor ot just list out everything...

(defun part-a (choice)
  (cond ((string-equal choice "A X") 4)
        ((string-equal choice "B X") 1)
        ((string-equal choice "C X") 7)
        ((string-equal choice "A Y") 8)
        ((string-equal choice "B Y") 5)
        ((string-equal choice "C Y") 2)
        ((string-equal choice "A Z") 3)
        ((string-equal choice "B Z") 9)
        ((string-equal choice "C Z") 6)))

(defun part-b (choice)
  (cond ((string-equal choice "A X") 3)
        ((string-equal choice "B X") 1)
        ((string-equal choice "C X") 2)
        ((string-equal choice "A Y") 4)
        ((string-equal choice "B Y") 5)
        ((string-equal choice "C Y") 6)
        ((string-equal choice "A Z") 8)
        ((string-equal choice "B Z") 9)
        ((string-equal choice "C Z") 7)))

(defun calc-score (fn choices)
  (reduce #'+ (mapcar #'(lambda (choice) (funcall fn choice))
                      choices)
          :initial-value 0))

(defun day-two ()
  (with-open-file (stream *input-file*)
    (let ((choices (advent-of-code:read-lines stream)))
        (print (list "A" (calc-score #'part-a choices)
                     "B" (calc-score #'part-b choices))))))

(day-two)
