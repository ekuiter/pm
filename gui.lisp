(defvar *filtered-projects* nil)

(defun require-package (package)
  (ltk:format-wish "package require ~a" package))

(defun configure-style (style &rest options)
  (ltk:format-wish "ttk::style configure ~a~{ ~a~}" style
		   (loop while options collect
			(format nil "-~(~a~) ~(~a~)" (pop options) (pop options)))))

(defun center-window (window width height)
  (ltk:minsize window width height)
  (ltk:maxsize window width height)
  (ltk:set-geometry window width height
		    (/ (- (ltk:screen-width) width) 2)
		    (/ (- (ltk:screen-height) height) 2)))

(defun image-property (image property)
  (ltk:format-wish "senddatastring [image ~a ~a]" property (ltk::name image))
  (parse-integer (ltk::read-data)))

(defun draw-image (canvas path &key (width nil width-p) (height nil height-p))
  (let ((image (ltk:make-image)))
    (ltk:format-wish "~a delete all" (ltk::widget-path canvas))
    (unless path (return-from draw-image))
    (ltk:create-image canvas 0 0 :image image)
    (ltk:image-load image path)
    (unless width (setf width (image-property image "width")))
    (unless height (setf height (image-property image "height")))
    (when width-p (ltk:configure canvas :width width))
    (when height-p (ltk:configure canvas :height height))))

(defun draw-image-resource (canvas resource)
  (draw-image canvas (when resource (format nil "res/~(~a~).png" resource))))

(defun listbox-size (listbox)
  (ltk:format-wish "senddatastring [~a size]" (ltk::widget-path listbox))
  (parse-integer (ltk::read-data)))

(defun move-listbox-selection (listbox direction)
  (let ((selection (first (ltk:listbox-get-selection listbox))))
    (unless selection (return-from move-listbox-selection))
    (let ((new-selection (if (equal direction :up)
			     (max 0 (- selection 1))
			     (min (- (listbox-size listbox) 1) (+ selection 1)))))
      (ltk:listbox-select listbox nil)
      (ltk:listbox-select listbox new-selection))))

(defun pack-frame (master &rest options)
  (let ((frame (make-instance 'ltk:frame :master master)))
    (apply 'ltk:pack frame :fill :both :anchor :nw options)
    frame))

(defun set-label-text (label text)
  (setf (ltk:text label) (if text text "")))

(defun get-binding-name (tree)
  (if (getf tree :gensymed)
      (getf tree :name)
      (intern (concatenate 'string
			   (symbol-name (getf tree :name)) "-"
			   (symbol-name (getf tree :type))))))

(defun preorder-traversal (tree)
  (let ((traversal))
    (labels ((traverse (tree parent)
	       (setf tree (macroexpand tree))
	       (setf tree (append (list :type) tree))
	       (setf (getf tree :children) (macroexpand (getf tree :children)))
	       (unless (getf tree :name)
		 (setf (getf tree :name) (gensym))
		 (setf (getf tree :gensymed) t))
	       (setf (getf tree :name) (get-binding-name tree))
	       (setf traversal (append traversal
				       (list (append tree (list :parent parent)))))
	       (loop for child in (getf tree :children) do
		    (traverse child (getf tree :name)))))
      (traverse tree nil)
      traversal)))

(defun make-widget-instance (tree)
  (let ((instance-sym (gensym)))
    (destructuring-bind (&key type name gensymed init pack grid configure children parent) tree
      (declare (ignore name gensymed children))
      (when (equal type 'frame)
	(setf pack (append pack '(:fill :both :anchor :nw))))
      `(let ((,instance-sym (make-instance ',(intern (symbol-name type) 'ltk)
					   :master ,parent ,@init)))
	 ,@(loop while configure collect
		`(ltk:configure ,instance-sym ,(pop configure) ,(pop configure)))
	 ,(if grid
	      `(ltk:grid ,instance-sym ,@grid)
	      `(ltk:pack ,instance-sym ,@pack))
	 ,instance-sym))))

(defun make-let-form (traversal forms)
  (if traversal
      (let* ((current-tree (car traversal))
	     (widget-name (getf current-tree :name)))
	`(let ((,widget-name ,(make-widget-instance current-tree)))
	   (declare (ignorable ,widget-name))
	   ,(make-let-form (cdr traversal) forms)))
      `(progn ,@forms)))

(defmacro let-widgets (tree &body forms)
  (make-let-form (preorder-traversal tree) forms))

(defmacro command (widget &rest forms)
  `(setf (ltk:command ,widget)
	 (if (equal (ltk::widget-class-name ,widget) "ttk::button")
	     (lambda () ,@forms)
	     (lambda (arg) (declare (ignorable arg)) ,@forms))))

(defmacro bind (widget &rest bindings)
  (loop for binding in bindings collect
       (destructuring-bind (event-name &rest forms) binding
	 `(ltk:bind ,widget ,event-name
		    (lambda (evt)
		      (declare (ignorable evt))
		      ,@forms)))
     into binding-forms finally
       (return `(progn ,@binding-forms))))

(defmacro details-grid (&rest fields)
  (loop for widget in fields for row from 0 to (- (length fields) 1)
     with widgets do
       (let ((left-label `(label :init (:text ,(getf widget :text))
				 :grid (,row 0 :sticky :e)
				 :configure (:style ,(getf widget :style "TLabel"))))
	     (right-label `(label :name ,(getf widget :name)
				  :grid (,row 1 :sticky :w))))
	 (setf widgets (append widgets (list left-label right-label))))
     finally (return widgets)))

(defmacro action-button (&key name text)
  `(button :name ,name
	   :init (:text ,text :width (length ,text))
	   :pack (:side :left :padx 1)
	   :configure (:takefocus 0)))

(defmacro command-remote-operation (button-name function-name verb)
  `(command ,button-name
	    (with-project (get-selected-project)
	      (let ((remote (first (project-remotes))))
		(ltk:do-msg (,function-name remote (project-current-branch))
		  :title (format nil "~a ~a (remote ~a, branch ~a)"
				 ,verb (project-name) remote (project-current-branch)))))))

(defun gui ()
  (ltk:with-ltk ()
    (ltk::use-theme "clam")
    (require-package "Img")
    (ltk:wm-title ltk:*tk* "pm")
    (center-window ltk:*tk* 500 300)
    (ltk:font-create :big-font :size 20)
    (ltk:font-create :small-font :size 13)
    (configure-style "TFrame" :background :white)
    (configure-style "TLabel" :background :white)
    (configure-style "TButton" :font :small-font)
    (configure-style "TButton" :padding 3)

    (let-widgets
	(frame :name wrapper
	       :pack (:expand t :padx 4 :pady 4) :children
	       ((frame :name left
		       :pack (:side :left) :children
		       ((entry :name search
			       :pack (:fill :both)
			       :configure (:font :big-font))
			(frame :pack (:pady 2))
			(listbox :name search
				 :pack (:expand t :fill :both)
				 :configure (:borderwidth 0 :takefocus 0))))
		(frame :pack (:side :left :padx 2))
		(frame :name right
		       :pack (:expand t :side :left) :children
		       ((frame :name title :children
			       ((canvas :name namespace
					:init (:width 32 :height 32)
					:pack (:side :left)
					:configure (:highlightthickness 0))
				(label :name name
				       :pack (:side :left :fill :both)
				       :configure (:font :big-font :padding "5 2"))))
			(frame :name details
			       :pack (:expand t :pady 2) :children
			       (details-grid (:name technology :text "Technology:")
					     (:name year :text "Year:")
					     (:name current-branch :text "Current branch:"
						    :style "GitDetails.TLabel")
					     (:name remotes :text "Remotes:"
						    :style "GitDetails.TLabel")))
			(frame :name actions
			       :pack (:fill :none :anchor :e) :children
			       ((action-button :name open :text "Open")
				(action-button :name open-in-terminal :text "Term")
				(action-button :name stats :text "Stats")
				(action-button :name push :text "Push")
				(action-button :name pull :text "Pull")))))))

      (labels ((get-selected-project ()
		 (let ((selection (first (ltk:listbox-get-selection search-listbox))))
		   (when *filtered-projects* (nth selection *filtered-projects*))))

	       (update-project ()
		 (with-project (get-selected-project)
		   (configure-style "TLabel" :foreground
				    (if (get-selected-project) :black :white))
		   (configure-style "GitDetails.TLabel" :foreground
				    (if (project-gitp) :black :white))
		   (ltk:configure stats-button :state (if (project-gitp) :normal :disabled))
		   (ltk:configure pull-button :state (if (project-gitp) :normal :disabled))
		   (ltk:configure push-button :state (if (project-gitp) :normal :disabled))
		   (ltk:focus search-entry)
		   (set-label-text name-label (project-name))
		   (set-label-text technology-label (project-technology))
		   (set-label-text year-label (project-year))
		   (set-label-text current-branch-label (project-current-branch))
		   (set-label-text remotes-label (format nil "~{~a~^, ~}" (project-remotes)))
		   (draw-image-resource namespace-canvas (project-namespace))))
	       
	       (filter-projects ()
		 (setf *filtered-projects* (search-projects (ltk:text search-entry)))
		 (ltk:listbox-clear search-listbox)
		 (ltk:listbox-append search-listbox
				     (mapcar #'project-directory *filtered-projects*))
		 (when *filtered-projects*	(ltk:listbox-select search-listbox 0))
		 (update-project))

	       (reset-filter ()
		 (setf (ltk:text search-entry) "")
		 (filter-projects)))

	(bind search-entry
	      ("<Key>"
	       (let ((given-char (aref (write-to-string (ltk:event-char evt)) 0)))
		 (when (alphanumericp given-char)
		   (filter-projects))))
	      ("<Key-Up>"
	       (move-listbox-selection search-listbox :up)
	       (update-project))
	      ("<Key-Down>"
	       (move-listbox-selection search-listbox :down)
	       (update-project))
	      ("<Return>"
	       (open-project (get-selected-project)))
	      ("<Command-a>"
	       (reset-filter)))

	(command search-listbox
		 (update-project))
	(command open-button
		 (open-project (get-selected-project)))
	(command open-in-terminal-button
		 (open-project-in-terminal (get-selected-project)))
	(command stats-button
		 (project-stats (get-selected-project)))
	(command-remote-operation push-button push-project "Push")
	(command-remote-operation pull-button pull-project "Pull")
	
	(reset-filter)))))