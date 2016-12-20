(defvar *filtered-projects* nil)

(defun configure-window (window title width height)
  (ltk:wm-title window title)
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

(defun gui ()
  (ltk:with-ltk ()
    (let* ((title-size 32)
	   (wrapper-frame (pack-frame (pack-frame nil) :padx 4 :pady 4))
	   (left-frame (pack-frame wrapper-frame :side :left))
	   (middle-frame (pack-frame wrapper-frame :side :left :padx 2))
	   (right-frame (pack-frame wrapper-frame))
	   (search-entry (make-instance 'ltk:entry :master left-frame))
	   (search-listbox (make-instance 'ltk:listbox :master left-frame))
	   (title-frame (pack-frame right-frame))
	   (namespace-canvas (make-instance 'ltk:canvas :master title-frame
					    :width title-size :height title-size))
	   (name-label (make-instance 'ltk:label :master title-frame))
	   (details-frame (pack-frame right-frame :pady 2))
	   (technology-label (make-instance 'ltk:label :master details-frame))
	   (year-label (make-instance 'ltk:label :master details-frame))
	   
	   (load-image-resource
	    (lambda (canvas resource)
		(draw-image canvas (when resource (format nil "res/~a.png" resource)))))

	   (get-selected-project
	    (lambda ()
	      (let ((selection (first (ltk:listbox-get-selection search-listbox))))
		(when *filtered-projects* (nth selection *filtered-projects*)))))

	   (update-project
	    (lambda ()
	      (with-project (funcall get-selected-project)
		(set-label-text name-label (project-name))
		(set-label-text technology-label (project-technology))
		(set-label-text year-label (project-year))
		(funcall load-image-resource namespace-canvas (project-namespace)))))
	   
	   (filter-projects
	    (lambda ()
	      (setf *filtered-projects* (search-projects (ltk:text search-entry)))
	      (ltk:listbox-clear search-listbox)
	      (ltk:listbox-append search-listbox
				  (mapcar #'project-directory *filtered-projects*))
	      (when *filtered-projects*	(ltk:listbox-select search-listbox 0))
	      (funcall update-project)))

	   (reset-filter
	    (lambda (&optional evt)
	      (declare (ignore evt))
	      (setf (ltk:text search-entry) "")
	      (funcall filter-projects))))

      (declare (ignore middle-frame))
      (ltk::use-theme "clam")
      (ltk:format-wish "package require Img")
      (ltk:format-wish "ttk::style configure TFrame -background white")
      (ltk:format-wish "ttk::style configure TLabel -background white")
      (configure-window ltk:*tk* "pm" 500 200)
      (ltk:font-create :big-font :size 20)

      (ltk:configure search-entry :font :big-font)
      (ltk:focus search-entry)
      (ltk:bind search-entry "<Key>"
		(lambda (evt)
		  (let ((given-char (aref (write-to-string (ltk:event-char evt)) 0)))
		    (when (alphanumericp given-char)
		      (funcall filter-projects)))))
      (ltk:bind search-entry "<Key-Up>"
		(lambda (evt)
		  (declare (ignore evt))
		  (move-listbox-selection search-listbox :up)
		  (funcall update-project)))
      (ltk:bind search-entry "<Key-Down>"
		(lambda (evt)
		  (declare (ignore evt))
		  (move-listbox-selection search-listbox :down)
		  (funcall update-project)))
      (ltk:bind search-entry "<Return>"
		(lambda (evt)
		  (declare (ignore evt))
		  (open-project (funcall get-selected-project))
		  (funcall reset-filter)))
      (ltk:bind search-entry "<Command-a>" reset-filter)

      (ltk:configure search-listbox :borderwidth 0)
      (ltk:configure search-listbox :takefocus 0)
      (setf (ltk:command search-listbox)
	    (lambda (sel)
	      (declare (ignore sel))
	      (ltk:focus search-entry)
	      (funcall update-project)))
      (funcall reset-filter)

      (ltk:configure name-label :font :big-font)
      (ltk:configure name-label :padding "5 2")

      (ltk:configure namespace-canvas :highlightthickness 0)

      (ltk:pack search-entry :fill :both)
      (pack-frame left-frame :pady 2)
      (ltk:pack search-listbox :fill :both)
      (ltk:pack namespace-canvas :side :left)
      (ltk:pack name-label :fill :both)
      (ltk:grid (make-instance 'ltk:label :master details-frame :text "Technology:") 0 0 :sticky :e)
      (ltk:grid technology-label 0 1 :sticky :w)
      (ltk:grid (make-instance 'ltk:label :master details-frame :text "Year:") 1 0 :sticky :e)
      (ltk:grid year-label 1 1 :sticky :w))))

(gui)