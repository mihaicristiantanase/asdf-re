;;;; utils.lisp

(in-package #:asdf-re)

(defun has-suffix (s suffix)
  (= (or (search suffix s :from-end t) -1)
     (- (length s) (length suffix))))

(defmacro fmt (format &body args)
  `(format nil ,format ,@args))

(defun fname (p)
  (car (last (cl-ppcre:split "/" (namestring p)))))
