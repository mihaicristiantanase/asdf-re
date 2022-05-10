;;;; asdf-re.lisp

(in-package #:asdf-re)

(defparameter *lisp-files-scanner*
  (cl-ppcre:create-scanner ".*\\.(lisp|asd)$" :case-insensitive-mode t))

(defun has-suffix (s suffix)
  (= (or (search suffix s :from-end t) -1)
     (- (length s) (length suffix))))

(defmacro fmt (format &body args)
  `(format nil ,format ,@args))

(defmacro with-lisp-file ((path) &body rest)
  (let ((stream (gensym)))
    `(with-open-file (,stream ,path :direction :output :if-exists :supersede)
       (flet ((wl (line)
                (format ,stream "~a~%" line)))
         ,@rest))))

(defun reconstruct (path target)
  (unless (has-suffix (namestring path) "/")
    (error "Missing \"/\" for ~a" path))
  (unless (has-suffix (namestring target) "/")
    (error "Missing \"/\" for ~a" target))

  (let* ((system-name (car (last (pathname-directory path))))
         (re-system-name (fmt "~a~a/" target system-name))
         (flisp (pathname (fmt "/tmp/~a.lisp" system-name)))
         )
    (with-lisp-file (flisp)
      (wl (fmt "(ql:quickload \"shell\")"))
      (wl (fmt "(shell:run t \"rm\" \"-rf\" \"~a\")" re-system-name))
      (wl (fmt "(shell:run t \"mkdir\" \"-p\" \"~a\")" re-system-name))
      (let ((files (remove-if-not
                    #'(lambda (it) (cl-ppcre:scan *lisp-files-scanner* (namestring it)))
                    (uiop:directory-files path))))
        (loop for f in files
              do (let ((fname (car (last (cl-ppcre:split "/" (namestring f))))))
                   (wl (fmt "~%(with-open-file (f \"~a~a\" :direction :output)" re-system-name fname))
                   (with-open-file (fsrc f)
                     (loop for line = (read-line fsrc nil nil)
                           while line
                           do (wl (fmt "  (format f \"~a~~%\")"
                                    (cl-ppcre:regex-replace-all "\"" line "\\\"")))))
                   (wl ")")))
        (format t "ASDF system reconstruction done!")))))
