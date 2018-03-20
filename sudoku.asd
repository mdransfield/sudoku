;;;; sudoku.asd

(asdf:defsystem #:sudoku
  :serial t
  :description "Sudoku solver"
  :author "Martin Dransfield <mdransfield@gmail.com>"
  :license "GNU GPL 3 (see file LICENSE for details"
  :components ((:file "package")
               (:file "sudoku")))

