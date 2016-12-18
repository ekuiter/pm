(defun interleave (&rest lists)
  "Interleaves list arguments in a zipped fashion."
  (let ((non-empty-lists (remove 'nil lists)))
    (cond ((not lists) nil) ; all lists are empty
	  ((equal lists non-empty-lists) ; all lists are populated
	   (append (mapcar 'car lists)
		   (apply 'interleave (mapcar 'cdr lists))))
	  (non-empty-lists ; some lists are populated
	   (apply 'interleave non-empty-lists)))))

(defun make-project-plist (project-path)
  "Splits a project directory into a property list."
  (let ((project-directory (project-directory project-path)))
    (when (and (char/= #\~ #\. (aref project-directory 0)) ; first character not "~"
	       (ccl:directoryp project-path)) ; has to be directory
      (multiple-value-bind (parts idx)
	  (split-sequence:split-sequence #\- project-directory ; split by hyphens
					 :count (- (length *project-directory-parts*) 1))
	(interleave *project-directory-parts* ; make the property list
		    (append parts (list (subseq project-directory idx))))))))

(defun sanitize-namespace (project)
  "Constructs proper namespace values."
  (let ((namespace (project-namespace project)))
    (cond ((equal namespace "pub") "public")
	  ((equal namespace "x") "private")
	  (t namespace))))

(defun sanitize-year (project)
  "Constructs proper year values."
  (+ 2000 (parse-integer (project-year project))))

(defun make-project (project-path)
  "Constructs a project object from a project path."
  (let ((project-plist (make-project-plist project-path)))
    (when project-plist
      (setf (getf project-plist :namespace) (sanitize-namespace project-plist))
      (setf (getf project-plist :year) (sanitize-year project-plist))
      (append (list :path project-path) project-plist))))