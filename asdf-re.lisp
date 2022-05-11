;;;; asdf-re.lisp

(in-package #:asdf-re)

(defparameter *lisp-files-scanner*
  (cl-ppcre:create-scanner "^[^.].*\\.(lisp|asd)$" :case-insensitive-mode t))

(defun valid-lisp-file? (path)
  (cl-ppcre:scan *lisp-files-scanner* (fname path)))

(defmacro with-lisp-file ((path) &body rest)
  (let ((stream (gensym)))
    `(with-open-file (,stream ,path :direction :output :if-exists :supersede)
       (flet ((wl (line)
                (format ,stream "~a~%" line)))
         ,@rest))))

(defun escape4format (s)
  (loop for tr in '(("\"" . "\\\"")
                    ("~" . "~~"))
        do (setf s (cl-ppcre:regex-replace-all (car tr) s (cdr tr)))
        finally (return s)))

(defun reconstruct (path target)
  (unless (has-suffix (namestring path) "/")
    (error "Missing \"/\" for ~a" path))
  (unless (has-suffix (namestring target) "/")
    (error "Missing \"/\" for ~a" target))

  (let* ((system-name (car (last (pathname-directory path))))
         (re-system-name (fmt "~a~a/" target system-name))
         (flisp (pathname (fmt "/tmp/~a.lisp" system-name))))
    (with-lisp-file (flisp)
      (wl (fmt "(ql:quickload \"shell\")"))
      (wl (fmt "(shell:run t \"rm\" \"-rf\" \"~a\")" re-system-name))
      (wl (fmt "(shell:run t \"mkdir\" \"-p\" \"~a\")" re-system-name))
      (let ((files (remove-if-not
                    #'(lambda (it) (valid-lisp-file? it))
                    (uiop:directory-files path))))
        (loop for f in files
              do (let ((fname (fname f)))
                   (wl (fmt "~%(with-open-file (f \"~a~a\" :direction :output)" re-system-name fname))
                   (with-open-file (fsrc f)
                     (loop for line = (read-line fsrc nil nil)
                           while line
                           do (wl (fmt "  (format f \"~a~~%\")" (escape4format line)))))
                   (wl ")")))
        (format t "ASDF system reconstruction done at ~a" flisp)))))
