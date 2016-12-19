(defun add-project-function (function-name)
  "Adds a function to the list of project-defun'ed functions."
  (unless (find function-name *project-functions*)
    (push function-name *project-functions*)))

(defmacro defaccessor (&rest accessors)
  "Accesses a project field."
  (append '(progn)
	  (loop for accessor in accessors collect
	       (let ((function-name ; construct the accessor function name
		      (intern (concatenate 'string "PROJECT-" (symbol-name accessor)))))
		 (append
		  `(progn (add-project-function ',function-name)) ; add project operation
		  `((defun ,function-name (project)
		      (getf project ,accessor)))))))) ; get value from property list

(defmacro project-defun (spec args &body body)
  "Defines a function that operates on a project given as the first argument."
  `(progn (add-project-function ',spec)
	  (defun ,spec ,(append '(project) args)
	    ,(if (equal (car body) :without-project)
		 `(progn ,@(cdr body))
		 `(with-project project ,@body)))))

(defun insert-project-to-form (project-sym)
  "Inserts a project argument to calls to project-defun'ed functions."
  (lambda (form)
    (if (atom form)
	form
	(append (list (car form))
		(when (find (car form) *project-functions*) (list project-sym))
		(mapcar (insert-project-to-form project-sym) (cdr form))))))

(defmacro with-project (p &body forms)
  "Allows the project argument to be omitted from calls to project-defun'ed functions."
  (let ((project-sym (gensym)))
    `(let ((,project-sym ,p)) (declare (ignorable ,project-sym))
	  ,@(mapcar (insert-project-to-form project-sym) forms))))