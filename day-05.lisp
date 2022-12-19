(require :advent-of-code)

(defparameter *input-file* "data/input_05.txt")


(defun partition-string (n str)
  (if (>= (length str) n)
      (let ((head (subseq str 0 n))
            (tail (subseq str n)))
         (cons head (partition-string n tail)))
      (list str)))


(defun create-stacks (stacks)
  (mapcar #'(lambda (stack) ;; cleaning up the list of lists of crate
              (advent-of-code:filter #'(lambda (crate)
                                         (let ((crate (string-trim '(#\space #\newline #\[ #\])
                                                                   crate)))
                                           (and (> (length crate) 0) crate)))
                                     stack))
          (advent-of-code:transpose  ;; create a list of lists of create from the input stacks
           (mapcar #'(lambda (stack)
                       (partition-string 4 stack))
                   stacks))))


(defun parse-instruction (instruction)
  (let ((parts (advent-of-code:split-string instruction)))
    (values (parse-integer (nth 1 parts))
            (- (parse-integer (nth 3 parts)) 1)
            (- (parse-integer (nth 5 parts)) 1))))


(defun move-single (instruction stacks)
  (multiple-value-bind (n a b) (parse-instruction instruction)
    (dotimes (i n)
        (push (pop (nth a stacks)) (nth b stacks)))))


(defun move-multiple (instruction stacks)
  (multiple-value-bind (n a b) (parse-instruction instruction)
    (let ((temp '()))
      (dotimes (i n)
        (push (pop (nth a stacks)) temp))
      (dotimes (i n)
        (push (pop temp) (nth b stacks))))))


(defun find-crates (fn lines)
  (let ((stacks (create-stacks (subseq lines 0 8)))
        (procedure (subseq lines 10)))
    (dolist (instruction procedure)
      (funcall fn instruction stacks))
    (concatenate 'string
                 (mapcar #'(lambda (stack)
                             (character (car stack)))
                         stacks))))


(defun day-five ()
  (with-open-file (stream *input-file*)
    (let ((lines (advent-of-code:read-lines stream)))
      (list "A" (find-crates #'move-single lines)
            "B" (find-crates #'move-multiple lines)))))

(day-five)
