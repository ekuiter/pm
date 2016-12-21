(defaccessor :namespace :technology :year :name)

(project-defun show-project (&key details)
  "Shows project information."
  (if details
      (format t "=========== ~a ===========~%~
                 Namespace:  ~a~%~
                 Technology: ~a~%~
                 Year:       ~a~%~
                 Path:       ~a~%~a~%"
	      (project-name) (project-namespace) (project-technology)
	      (project-year) (project-path) (project-git-details))
      (format t "~a~%" (project-directory))))

(project-defun project-path ()
  "Returns the path from a project."
  (if project
      (getf project :path)
      *projects-path*))

(project-defun project-directory () :without-project
  "Returns the directory from a project or project path."
  (cond ((not project) nil)
	((listp project) (project-directory (project-path project)))
	(t (nth 0 (last (pathname-directory project)))))) ; last path component

(defun export-path-command (&rest path-exports)
  "Returns a command that exports additional paths."
  (if path-exports
      (format nil "export PATH=$PATH:~{~a~^:~};" path-exports)
      ""))

(defun cd-command (path)
  (concatenate 'string "cd " (namestring path) ";"))

(project-defun run (command)
  "Runs a command in a specified project's path."
  (let ((shell-command (concatenate 'string (apply 'export-path-command *path-exports*)
				    (cd-command (project-path)) command)))
    (string-right-trim '(#\Linefeed)
		       (with-output-to-string (stream)
			 (ccl:run-program "sh" (list "-c" shell-command) :output stream)))))

(project-defun open-project ()
  "Reveals a project in Finder."
  (run "open ."))

(project-defun open-project-in-terminal ()
  "Opens a project in a terminal."
  (run (concatenate 'string "open . -a " *terminal-app*)))

(project-defun project-gitp ()
  "Returns whether the project has a git repository."
  (not (search "fatal" (run "git rev-parse HEAD"))))

(project-defun run-git (command)
  (when (project-gitp)
    (run (concatenate 'string "git " command))))

(project-defun project-current-branch ()
  (run-git "rev-parse --abbrev-ref HEAD"))

(project-defun project-remotes ()
  "Returns a list of remotes associated with the project."
  (when (project-gitp)
    (remove-if (lambda (remote) (equal remote ""))
	       (split-sequence:split-sequence #\Linefeed (run-git "remote")))))

(defmacro define-remote-operation (function-name verb)
  "Defines a function for pulling or pushing a repository."
  `(project-defun ,function-name (&optional (remote "") (branch ""))
     (run-git (concatenate 'string ,verb " " remote " " branch))))

(define-remote-operation pull-project "pull")
(define-remote-operation push-project "push")

(project-defun project-stats ()
  "Shows statistics for a project."
  (when (project-gitp)
    (run "gitstats . ../.stats")
    (run "open ../.stats/index.html")))

(project-defun project-git-details ()
  (if (project-gitp)
      (format nil "Branch:     ~a~%~
                   Remotes:    ~{~a~^, ~}~%"
	      (project-current-branch) (project-remotes))
      ""))