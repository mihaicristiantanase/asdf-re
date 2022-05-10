;;;; asdf-re.asd

(asdf:defsystem #:asdf-re
  :description "Reconstruct an ASDF system from one Lisp file"
  :author "Mihai Cristian Tănase"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:shell #:cl-ppcre)
  :components ((:file "package")
               (:file "asdf-re")))
