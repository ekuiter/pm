(defun projects ()
  "Returns all projects in the projects path."
  (if *projects* *projects*
      (setf *projects*
	    (loop for project-path in (cl-fad:list-directory *projects-path*)
	       collect (make-project project-path) into projects
	       finally (return (sort (remove 'nil projects) #'> :key #'project-year))))))

(defun show-projects (&key (projects (projects)) details)
  "Shows projects information."
  (loop for project in projects do
       (show-project project :details details)))

(defun find-projects (selector-fn)
  "Returns a list of projects matching a selector function."
  (remove-if-not selector-fn (projects)))

(defun search-projects (query)
  "Returns a list of projects matching a search query."
  (loop for project in (projects) collect
       (when (search query (project-name project)) project)
     into projects
     finally (return (remove 'nil projects))))

(defmacro define-singular-operation (function-name list-function-name)
  "Defines a function that returns one element instead of a list."
  `(defun ,function-name (arg)
     (car (,list-function-name arg))))

(define-singular-operation find-project find-projects)
(define-singular-operation search-project search-projects)

(defmacro where (&rest clauses)
  "Returns a selector function for specified criteria."
  `(lambda (project)
     (and ,@(loop while clauses
	       collect `(equal (getf project ,(pop clauses)) ,(pop clauses))))))