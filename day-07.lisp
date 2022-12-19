(require :advent-of-code)

(defparameter *input-file* "data/input_07.txt")

(defparameter *tree* nil)
(defparameter *cwd* nil)

(defstruct (node (:print-function
                  (lambda (n s d)
                    (format s "#<~A> ~D" (node-elt n) (node-data n)))))
  elt (data nil) (parent nil) (child nil) (sibling nil))


(defun make-tree (elements)
  (if (> (length elements) 0)
      (let ((element (car elements))
            (siblings (cdr elements)))
        (if (listp element)
          ;; if its a list it has children
          (let ((children (cdr element)))
            (make-node :elt (car element)
                       :child (make-tree children)
                       :sibling (make-tree siblings)))
          ;; else continue with siblings
          (make-node :elt element
                     :sibling (make-tree siblings))))))


;; ;; (setf *tree* (make-tree '("A" "B" ("C" "D") "E" ("F" "G" "H"))))
;; (setf *tree* nil)
;; (setf *tree* (make-tree '("A" "B")))

;; (node-child *tree*)                                 ;; => nil
;; (node-sibling *tree*)                               ;; => B
;; (node-sibling (node-child (node-sibling *tree*)))   ;; => D
;; (node-child (node-sibling (node-sibling *tree*)))   ;; => D
;; (node-child   ;; => G
;;    (print (node-sibling ;; => F
;;            (print (node-sibling ;; => E
;;                    (print (node-sibling ;; => C
;;                            (print (node-sibling *tree*))))))))) ;; => B

(defun has-child? (parent)
  (node-child parent))

(defun insert-child (obj data parent)
  (let ((first-child (node-child parent)))
    (if first-child
      (setf (node-sibling first-child) (make-node :elt obj :data data))
      (setf first-child (make-node :elt obj :data data)))))

(defun find-last-sibling (first-child)
  (if (node-sibling first-child)
      (find-last-sibling (node-sibling first-child))))

(defun find-siblings (child)
  (let ((sibling (node-sibling child)))
    (if sibling
        (cons sibling (find-siblings sibling)))))

(defun find-children (parent)
  (let ((first-child (node-child-parent)))
    (if first-child
        (cons first-child (find-siblings first-child)))))


;; (defparameter *cwd* nil)
;; (setf *cwd* (node-sibling *tree*))

;; (print *cwd*)

;; (insert-child "C" *cwd*)

;; (node-child (node-sibling *tree*))

;; (insert-child "D" *cwd*)

;; (setf (node-sibling (node-child *cwd*)) (make-node :elt "D"))

;; (node-sibling               ;; => D
;;   (node-child               ;; => C
;;     (node-sibling *tree*))) ;; => B

(defun find-sibling (elt sibling)
  (let ((the-sibling (node-sibling sibling)))
    (if the-sibling
        (if (equal (node-elt the-sibling) elt)
            the-sibling
            (find-sibling elt the-sibling)))))

(defun find-child (elt parent)
  (let ((first-child (node-child parent)))
    (if first-child
        (if (equal (node-elt first-child) elt)
            first-child
            (find-sibling elt first-child)))))

(defun change-dir (to cwd)
   (if (equal (node-elt cwd) to)
       cwd
       (if (equal ".." to)
           (let ((parent (node-parent cwd)))
             (if parent
                 parent
                 cwd))
           (find-child to cwd))))

(setf *tree* (make-node :elt "/"))

(defun execute (lines)
    (let ((cwd nil))
      (setf cwd *tree*)
      (dolist (line lines)
        (print cwd)
        ;; (print (node-child cwd))
        (if (dollarp line)
          ;; user commands
          (multiple-value-bind (command instruction)
              (values-list (cdr (advent-of-code:split-string line)))
            (if (equal "cd" command)
              (setf cwd (change-dir instruction cwd))))
          ;; machine output
          (multiple-value-bind (data name)
              (values-list (advent-of-code:split-string line))
            (if (not (node-child cwd))
                (setf (node-child cwd)
                      (make-node :elt name :data data))
                (setf (node-sibling (node-child cwd))
                      (make-node :elt name :data data))))))))


(defun dollarp (str)
  (let ((i (position #\$ str)))
    (if i
        (= i 0))))


(defun dirp (str)
  (let ((pos (find-substr-position "dir" str)))
    (if (numberp pos)
        (= pos 0))))


(defun process-line (str)
  (if (dollarp str)
      nil;; user command -> traverse the tree
      (multiple-value-bind (size name) (values-list advent-of-code:split-string str)
        (if (dirp size)
            (make-node name nil  tree)
            (make-node name size tree)))))


(defun sequentialp (numbers)
  (if (> (length numbers) 1)
    (every #'(lambda (pair)
                 (multiple-value-bind (a b) (values-list pair)
                   (if (and a b)
                       (= (- b a) 1))))
             (advent-of-code:chunk 2 numbers))
    t))


(defun find-substr-position (substr string)
  (let ((char-positions (mapcar #'(lambda (c)
                               (position c string))
                           (coerce substr 'list))))
    (if (sequentialp char-positions)
        (car char-positions))))

(find-substr-position "dir" "dir a")

(defun day-seven ()
  (with-open-file (stream *input-file*)
    (let ((lines (advent-of-code:read-lines stream)))
      (list "A" (execute (print (subseq lines 0 6)))
            "B" ))))

(day-seven)

(print (node-sibling (node-child *tree*)))
