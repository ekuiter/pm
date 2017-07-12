(in-package :gui)

(defvar *filtered-projects* nil)

(defun draw-image-resource (canvas resource)
  (draw-image canvas (when resource (format nil "res/~(~a~).png" resource))))

(defmacro action-button (&key name text)
  `(button :name ,name
	   :init (:text ,text :width (length ,text))
	   :pack (:side :left :padx 1)
	   :configure (:takefocus 0)))

(defmacro command-remote-operation (button-name function-name verb)
  `(command ,button-name
	    (let* ((project (get-selected-project))
		   (remote (first (remotes project))))
	      (ltk:do-msg (,function-name project remote (git-current-branch project))
		:title (format nil "~a ~a (remote ~a, branch ~a)"
			       ,verb (name project)
			       remote (git-current-branch project))))))

(defun exit-ltk ()
  (setf ltk:*exit-mainloop* t))

(defun gui ()
  (ltk:with-ltk ()
    (ltk::use-theme "clam")
    (require-package "Img")
    (ltk:wm-title ltk:*tk* "pm")
    (center-window ltk:*tk* 500 300)
    (ltk:font-create :big-font :size 20)
    (ltk:font-create :medium-font :size 15)
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
				 :configure (:borderwidth 0 :takefocus 0 :font :medium-font))))
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
		   (if *filtered-projects*
		       (nth selection *filtered-projects*)
		       *empty-project*)))

	       (update-project ()
		 (let ((project (get-selected-project)))
		   (configure-style "TLabel" :foreground
				    (if (emptyp project) :white :black))
		   (configure-style "GitDetails.TLabel" :foreground
				    (if (gitp project) :black :white))
		   (ltk:configure stats-button :state (if (gitp project) :normal :disabled))
		   (ltk:configure pull-button :state (if (gitp project) :normal :disabled))
		   (ltk:configure push-button :state (if (gitp project) :normal :disabled))
		   (ltk:focus search-entry)
		   (set-label-text name-label (name project))
		   (set-label-text technology-label (technology project))
		   (set-label-text year-label (if (eq (year project) 0) "" (year project)))
		   (set-label-text current-branch-label (git-current-branch project))
		   (set-label-text remotes-label (format nil "~{~a~^, ~}" (remotes project)))
		   (draw-image-resource namespace-canvas (namespace project))))
	       
	       (filter-projects ()
		 (let ((filter (ltk:text search-entry)))
		   (setf *filtered-projects*
			 (if (equal filter "")
			     (find-projects (where :not namespace "private"))
			     (search-projects filter))))
		 (ltk:listbox-clear search-listbox)
		 (ltk:listbox-append search-listbox
				     (mapcar #'name *filtered-projects*))
		 (when *filtered-projects* (ltk:listbox-select search-listbox 0))
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
	       (open-in-finder (get-selected-project))
	       (exit-ltk))
	      ("<Shift-Return>"
	       (open-in-terminal (get-selected-project))
	       (exit-ltk))
	      ("<Command-Return>"
	       (open-in-finder (get-selected-project))
	       (open-in-terminal (get-selected-project))
	       (exit-ltk))
	      ("<Command-a>" (reset-filter))
	      ("<Escape>" (exit-ltk)))

	(command search-listbox
		 (update-project))
	(command open-button
		 (open-in-finder (get-selected-project)))
	(command open-in-terminal-button
		 (open-in-terminal (get-selected-project)))
	(command stats-button
		 (git-stats (get-selected-project)))
	(command-remote-operation push-button git-push "Push")
	(command-remote-operation pull-button git-pull "Pull")
	
	(reset-filter)))))
