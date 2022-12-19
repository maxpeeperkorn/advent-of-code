(in-package :advent-of-code)

;; MACROS

(defmacro if-let ((var test-form) then-form &optional else-form)
  `(let ((,var ,test-form))
     (if ,var ,then-form, else-form)))


(defmacro -> (x form &rest forms)
  "Threads the expression through the forms. Inserts x as first item
   in the first form. Then, the result in the next form, until all
   forms are consumed."
  (if form
      (let ((threaded (if (listp form)
                        `(,(first form) ,x ,@(rest form))
                        (list form x))))
        `(->> ,threaded ,(first forms) ,@(rest forms)))
      x))


(defmacro ->> (x form &rest forms)
  "Threads the expression through the forms. Inserts x as last item
   in the first form. Then, the result in the next form, until all
   forms are consumed."
  (if form
      (let ((threaded (if (listp form)
                        `(,(first form) ,@(rest form) ,x)
                        (list form x))))
        `(->> ,threaded ,(first forms) ,@(rest forms)))
      x))

;; FUNCTIONS

(defun read-lines (stream)
  (let ((line (read-line stream nil :eof)))
    (if (not (eql line :eof))
        (cons line (read-lines stream)))))


(defun split-string (str &key (delimiter #\space))
  (if-let (i (position delimiter str))
    (cons (subseq str 0 i)
          (split-string (subseq str (+ i 1))
                        :delimiter delimiter))
    (list str)))


(defun filter (fn list)
  (let ((acc nil))
    (dolist (x list)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))


(defun take (n list)
  (when (and (not (zerop n)) list)
    (cons (first list) (take (- n 1) (cdr list)))))


(defun partition (n list)
  (if list
    (cons (take n list)
          (partition n (nthcdr n list)))))

(defun chunk (n list)
  (if (and (>= (length list) n) list)
      (cons (take n list)
            (chunk n (cdr list)))))

(defun range (from to &optional (step 1))
  (when (not (= from to))
    (cons from (range (+ from step) to step))))


(defun transpose (list-of-lists)
  (apply #'mapcar #'list list-of-lists))


(defun chunk-list (list &optional (n 1))
  (if (not (< (length list) n))
    (cons (take n list)
          (chunk-list list (+ n 1)))))

(defun monotonic (order-fn numbers)
  (reduce #'(lambda (acc n)
              (if (and acc (funcall order-fn n acc))
                  n nil))
          numbers))

