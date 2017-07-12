(in-package :project)

(defclass project-source ()
  ((path :reader path :initarg :path :initform (error "must supply path"))
   (name :reader name :initarg :name :initform nil)))

(defclass simple-source (project-source) ())
(defclass namespaced-source (project-source) ())
(defclass bare-source (project-source)
  ((exclude :reader exclude :initarg :exclude :initform nil)
   (extract-year :reader extract-year :initarg :extract-year :initform (constantly 0))))

(defclass project ()
  ((namespace :reader namespace :initarg :namespace :initform nil)
   (technology :reader technology :initarg :technology :initform nil)
   (year :reader year :initarg :year :initform nil)
   (name :reader name :initarg :name :initform nil)
   (path :reader path :initarg :path :initform nil)
   (source :reader source :initarg :source :initform nil)
   gitp))

(defclass file-project (project) ())

(defun study-extract-year (directory)
  (multiple-value-bind (parts)
      (split-sequence:split-sequence #\- directory)
    (parse-integer (first parts))))

(defparameter *sources*
  (list (make-instance 'namespaced-source :path #P"~/Projekte")
	(make-instance 'bare-source :path #P"~/Documents/Studium" :name "Studium"
		       :exclude (list "Unterlagen") :extract-year #'study-extract-year)
	(make-instance 'simple-source :path #P"~/Projekte/~drawings" :name "drawings")
	(make-instance 'simple-source :path #P"~/Projekte/~music" :name "music")
	(make-instance 'namespaced-source :path #P"~/Projekte/~videos" :name "videos")))
(defparameter *path-exports* '(#P"/usr/local/bin/"))
(defparameter *terminal-app* "/Applications/iTerm.app")
(defvar *all-projects* nil)
(defvar *empty-project* (make-instance 'project))

;;; PROJECT SOURCES

  (defmethod print-object ((source project-source) stream)
  (print-unreadable-object (source stream :type t)
    (format stream "~a" (path source))))

(defmethod make-project :around ((source project-source) (path pathname) &key)
  "Creates a project instance from a pathname regarding a source."
  (handler-case
      (let ((dirp (ccl:directoryp path)))
	(call-next-method source path
			  :directory (if dirp (project-directory path) (file-namestring path))
			  :class (if dirp 'project 'file-project)))
    (error ())))

(defmethod make-project ((source simple-source) (path pathname) &key directory class)
  (multiple-value-bind (parts idx)
      (split-sequence:split-sequence #\- directory :count 2) ; split by 2 hyphens
    (make-instance class
		   :namespace "private"
		   :technology (first parts)
		   :year (normalize-year (second parts))
		   :name (normalize-name (subseq directory idx) source)
		   :path path :source source)))

(defmethod make-project ((source namespaced-source) (path pathname) &key directory class)
  (multiple-value-bind (parts idx)
      (split-sequence:split-sequence #\- directory :count 3) ; split by 3 hyphens
    (make-instance class
		   :namespace (normalize-namespace (first parts))
		   :technology (second parts)
		   :year (normalize-year (third parts))
		   :name (normalize-name (subseq directory idx) source)
		   :path path :source source)))

(defmethod make-project ((source bare-source) (path pathname) &key directory class)
  (when (and (char/= #\. (aref directory 0))
	     (not (find directory (exclude source) :test #'equal)))
    (make-instance class
		   :namespace "private"
		   :technology "" :year (funcall (extract-year source) directory)
		   :name (normalize-name directory source)
		   :path path :source source)))

;;; PROJECTS

(defmethod show ((project (eql *empty-project*)) &key details)
  (declare (ignore details)))
(defmethod project-directory ((project (eql *empty-project*))))
(defmethod run ((project (eql *empty-project*)) command)
  (declare (ignore command)))
(defmethod open-in-finder ((project (eql *empty-project*))))
(defmethod open-in-terminal ((project (eql *empty-project*))))
(defmethod open-in-aquamacs ((project (eql *empty-project*))))
(defmethod gitp ((project (eql *empty-project*))))
(defmethod git-run ((project (eql *empty-project*)) command)
  (declare (ignore command)))
(defmethod git-current-branch ((project (eql *empty-project*))))
(defmethod remotes ((project (eql *empty-project*))))
(defmethod git-stats ((project (eql *empty-project*))))
(defmethod git-details ((project (eql *empty-project*))) "")
(defmethod emptyp ((project (eql *empty-project*))) t)
(defmethod emptyp ((project project)) nil)

(defmethod run ((project file-project) command)
  (declare (ignore command)))
(defmethod open-in-finder ((project file-project))
  (ccl:run-program "open" (list (namestring (path project)))))
(defmethod open-in-terminal ((project file-project))
  (open-in-finder project))
(defmethod open-in-aquamacs ((project file-project))
  (open-in-finder project))
(defmethod gitp ((project file-project)))
(defmethod git-run ((project file-project) command)
  (declare (ignore command)))
(defmethod git-current-branch ((project file-project)))
(defmethod remotes ((project file-project)))
(defmethod git-stats ((project file-project)))
(defmethod git-details ((project file-project)) "")

(defmethod print-object ((project project) stream)
  (print-unreadable-object (project stream :type t)
    (format stream "~a" (name project))))

(defun normalize-namespace (namespace)
  "Constructs proper namespace values."
  (cond ((equal namespace "pub") "public")
	((equal namespace "x") "private")
	(t namespace)))

(defun normalize-year (year)
  "Constructs proper year values."
  (+ 2000 (parse-integer year)))

(defun normalize-name (name source)
  "Constructs proper name values."
  (if (name source)
      (concatenate 'string "[" (name source) "] " name)
      name))

;;; PROJECT SEARCH & FILTERING

(defmethod projects ((source project-source))
  "Returns all projects in the source's path."
  (loop for path in (cl-fad:list-directory (path source))
     collect (make-project source path) into projects
     finally (return (sort (remove 'nil projects) #'> :key #'year))))

(defun all-projects ()
  "Returns all projects of all sources."
  (or *all-projects*
      (setf *all-projects* (loop for source in *sources* nconc (projects source)))))

(defun show-projects (&key (projects (all-projects)) details)
  "Shows projects information."
  (loop for project in projects do
       (show project :details details)))

(defun find-projects (selector-fn &key (projects (all-projects)))
  "Returns a list of projects matching a selector function."
  (remove-if-not selector-fn projects))

(defun search-projects (query &key (projects (all-projects)))
  "Returns a list of projects matching a search query."
  (setf query (substitute #\- #\Space query))
  (loop for project in projects
     when (search query (name project) :test #'equalp)
     collect project))

(defmacro define-singular-operation (function-name list-function-name)
  "Defines a function that returns one element instead of a list."
  `(defun ,function-name (arg &key (projects (all-projects)))
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

(defmethod project-directory ((project file-project))
  "Returns the file name from a file project."
  (file-namestring (path project)))

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

(defmethod open-in-aquamacs ((project project))
  "Opens a project in Aquamacs."
  (run project "aquamacs .")
  (sleep 5)
  (run project "osascript -e 'tell application \"Aquamacs\"
    activate
    tell application \"System Events\"
        key code 53 # Escape
        keystroke \"x\" # M-x
        keystroke \"neotree\"
        key code 36 # Return
        key code 53
        keystroke \"x\"
        keystroke \"projectile-mode\"
        key code 36
        keystroke \"x\" using control down
        keystroke \"g\"
    end tell
end tell'"))

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
