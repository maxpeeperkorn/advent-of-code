(defpackage advent-of-code-system
  (:use :cl :asdf))

(in-package :advent-of-code-system)


(defsystem "advent-of-code"
  :name "Advent of Code"
  :author "Max Peeperkorn"
  :version "2022"
  :description ""
  :serial t
  :components ((:file "package")
               (:file "utils")))
