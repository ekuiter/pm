(in-package :project)

(defparameter *projects-path* #P"~/Projekte/")
(defparameter *path-exports* '(#P"/usr/local/bin/"))
(defparameter *terminal-app* "/Applications/iTerm.app")
(defconstant +directory-parts+ 4)
(defvar *projects* nil)

(defclass project ()
  ((namespace :reader namespace :initarg :namespace :initform nil)
   (technology :reader technology :initarg :technology :initform nil)
   (year :reader year :initarg :year :initform nil)
   (name :reader name :initarg :name :initform nil)
   (path :reader path :initarg :path :initform nil)
   gitp))

(defvar *empty-project* (make-instance 'project))

(defmethod show ((project (eql *empty-project*)) &key details)
  (declare (ignore details)))
(defmethod project-directory ((project (eql *empty-project*))))
(defmethod run ((project (eql *empty-project*)) command)
  (declare (ignore command)))
(defmethod open-in-finder ((project (eql *empty-project*))))
(defmethod open-in-terminal ((project (eql *empty-project*))))
(defmethod gitp ((project (eql *empty-project*))))
(defmethod git-run ((project (eql *empty-project*)) command)
  (declare (ignore command)))
(defmethod git-current-branch ((project (eql *empty-project*))))
(defmethod remotes ((project (eql *empty-project*))))
(defmethod git-stats ((project (eql *empty-project*))))
(defmethod git-details ((project (eql *empty-project*))))
(defmethod emptyp ((project (eql *empty-project*))) t)
(defmethod emptyp ((project project)) nil)

(defmethod print-object ((project project) stream)
  (print-unreadable-object (project stream :type t)
    (format stream "~a" (name project))))

;;; PROJECT CREATION

(defun normalize-namespace (namespace)
  "Constructs proper namespace values."
  (cond ((equal namespace "pub") "public")
	((equal namespace "x") "private")
	(t namespace)))

(defun normalize-year (year)
  "Constructs proper year values."
  (+ 2000 (parse-integer year)))

(defun make-project (project-path)
  "Splits a project directory into a property list."
  (let ((project-directory (project-directory project-path)))
    (when (and (char/= #\~ #\. (aref project-directory 0)) ; first character not "~"
	       (ccl:directoryp project-path)) ; has to be directory
      (multiple-value-bind (parts idx)
	  (split-sequence:split-sequence #\- project-directory ; split by hyphens
					 :count (- +directory-parts+ 1))
	(make-instance 'project
		       :namespace (normalize-namespace (first parts))
		       :technology (second parts)
		       :year (normalize-year (third parts))
		       :name (subseq project-directory idx)
		       :path project-path)))))

;;; PROJECT SEARCH & FILTERING

(defun projects ()
  "Returns all projects in the projects path."
  (or *projects*
      (setf *projects*
	    (loop for project-path in (cl-fad:list-directory *projects-path*)
	       collect (make-project project-path) into projects
	       finally (return (sort (remove 'nil projects) #'> :key #'year))))))

(defun show-projects (&key (projects (projects)) details)
  "Shows projects information."
  (loop for project in projects do
       (show project :details details)))

(defun find-projects (selector-fn &key (projects (projects)))
  "Returns a list of projects matching a selector function."
  (remove-if-not selector-fn projects))

(defun search-projects (query &key (projects (projects)))
  "Returns a list of projects matching a search query."
  (loop for project in projects
     when (search query (name project))
     collect project))

(defmacro define-singular-operation (function-name list-function-name)
  "Defines a function that returns one element instead of a list."
  `(defun ,function-name (arg &key (projects (projects)))
     (car (,list-function-name arg :projects projects))))

(define-singular-operation find-project find-projects)
(define-singular-operation search-project search-projects)

(defmacro where (&rest clauses)
  "Returns a selector function for specified criteria."
  (labels ((clause ()
		`(equal (slot-value project ',(pop clauses)) ,(pop clauses))))
    `(lambda (project)
       (and ,@(loop while clauses
		 collect (if (eql (car clauses) :not)
			     (progn (pop clauses) `(not ,(clause)))
			     (clause)))))))

;;; PROJECT OPERATIONS

(defmethod show ((project project) &key details)
  "Shows project information."
  (if details
      (format t "=========== ~a ===========~%~
                 Namespace:  ~a~%~
                 Technology: ~a~%~
                 Year:       ~a~%~
                 Path:       ~a~%~a~%"
	      (name project) (namespace project) (technology project)
	      (year project) (path project) (git-details project))
      (format t "~a~%" (project-directory project))))

(defmethod project-directory ((path pathname))
  "Returns the directory from a project path."
  (nth 0 (last (pathname-directory path)))) ; last path component

(defmethod project-directory ((project project))
  "Returns the directory from a project."
  (project-directory (path project)))

(defun export-path-command (&rest path-exports)
  "Returns a command that exports additional paths."
  (if path-exports
      (format nil "export PATH=$PATH:~{~a~^:~};" path-exports)
      ""))

(defun cd-command (path)
  (concatenate 'string "cd " (namestring path) ";"))

(defmethod run ((project project) command)
  "Runs a command in a specified project's path."
  (let ((shell-command (concatenate 'string (apply 'export-path-command *path-exports*)
				    (cd-command (path project)) command)))
    (string-right-trim '(#\Linefeed)
		       (with-output-to-string (stream)
			 (ccl:run-program "sh" (list "-c" shell-command) :output stream)))))

(defmethod open-in-finder ((project project))
  "Reveals a project in Finder."
  (run project "open ."))

(defmethod open-in-terminal ((project project))
  "Opens a project in a terminal."
  (run project (concatenate 'string "open . -a " *terminal-app*)))

(defmethod gitp ((project project))
  "Returns whether the project has a git repository."
  (if (slot-boundp project 'gitp)
      (slot-value project 'gitp)
      (setf (slot-value project 'gitp)
	    (not (search "fatal" (run project "git rev-parse HEAD"))))))

(defmethod git-run ((project project) command)
  (when (gitp project)
    (run project (concatenate 'string "git " command))))

(defmethod git-current-branch ((project project))
  (git-run project "rev-parse --abbrev-ref HEAD"))

(defmethod remotes ((project project))
  "Returns a list of remotes associated with the project."
  (when (gitp project)
    (remove-if (lambda (remote) (equal remote ""))
	       (split-sequence:split-sequence #\Linefeed (git-run project "remote")))))

(defmacro define-remote-operation (function-name verb)
  "Defines a function for pulling or pushing a repository."
  `(progn
     (defmethod ,function-name ((project (eql *empty-project*)) &optional (remote "") (branch ""))
       (declare (ignore remote branch)))
     (defmethod ,function-name ((project project) &optional (remote "") (branch ""))
       (git-run project (concatenate 'string ,verb " " remote " " branch)))))

(define-remote-operation git-pull "pull")
(define-remote-operation git-push "push")

(defmethod git-stats ((project project))
  "Shows statistics for a project."
  (when (gitp project)
    (run project "gitstats . ../.stats")
    (run project "open ../.stats/index.html")))

(defmethod git-details ((project project))
  (if (gitp project)
      (format nil "Branch:     ~a~%~
                   Remotes:    ~{~a~^, ~}~%"
	      (git-current-branch project) (remotes project))
      ""))
