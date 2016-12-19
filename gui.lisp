(defvar *projects-in-listbox* nil)

(defun configure-window (window title width height)
  (ltk:wm-title window title)
  (ltk:minsize window width height)
  (ltk:maxsize window width height)
  (ltk:set-geometry window width height
		    (/ (- (ltk:screen-width) width) 2)
		    (/ (- (ltk:screen-height) height) 2)))

(defun draw-image (image &key master width height)
  (let ((canvas (make-instance 'ltk:canvas :master master :width width :height height)))
    (ltk:format-wish "package require Img")
    (ltk:create-image canvas 0 0 :image image)
    canvas))

(defun image-property (image property)
  (ltk:format-wish "senddatastring [image ~a ~a]" property (ltk::name image))
  (parse-integer (ltk::read-data)))

(defun load-image (canvas image path &key width height)
  (ltk:image-load image path)
  (unless width (setf width (image-property image "width")))
  (unless height (setf height (image-property image "height")))
  (ltk:configure canvas :width width)
  (ltk:configure canvas :height height))

(defun listbox-size (listbox)
  (ltk:format-wish "senddatastring [~a size]" (ltk::widget-path listbox))
  (parse-integer (ltk::read-data)))

(defun move-listbox-selection (listbox direction)
  (lambda (evt)
    (declare (ignore evt))
    (let* ((selection (first (ltk:listbox-get-selection listbox)))
	   (new-selection (if (equal direction :up)
			      (max 0 (- selection 1))
			      (min (- (listbox-size listbox) 1) (+ selection 1)))))
      (ltk:listbox-select listbox nil)
      (ltk:listbox-select listbox new-selection))))

(defun show-projects-in-listbox (projects listbox)
  (setf *projects-in-listbox* projects)
  (ltk:listbox-clear listbox)
  (ltk:listbox-append listbox (mapcar #'project-directory projects))
  (when projects (ltk:listbox-select listbox 0)))

(defun gui ()
  (ltk:with-ltk ()
    (let* ((frame (make-instance 'ltk:frame))
	   (search-entry (make-instance 'ltk:entry))
	   (search-listbox (make-instance 'ltk:listbox :master frame)))

      (configure-window ltk:*tk* "pm" 400 200)

      (ltk:font-create :search-font :size 20)
      (ltk:configure search-entry :font :search-font)
      (ltk:focus search-entry)
      (ltk:bind search-entry "<Key>"
		(lambda (evt)
		  (let ((given-char (aref (write-to-string (ltk:event-char evt)) 0)))
		    (when (alphanumericp given-char)
		      (show-projects-in-listbox (search-projects (ltk:text search-entry))
						search-listbox)))))
      (ltk:bind search-entry "<Key-Up>" (move-listbox-selection search-listbox :up))
      (ltk:bind search-entry "<Key-Down>" (move-listbox-selection search-listbox :down))
      (ltk:bind search-entry "<Return>"
		(lambda (evt)
		  (declare (ignore evt))
		  (let ((selection (first (ltk:listbox-get-selection search-listbox))))
		    (open-project (nth selection *projects-in-listbox*)))))

      (ltk:configure search-listbox :borderwidth 0)
      (setf (ltk:command search-listbox)
	    (lambda (sel)
	      (declare (ignore sel))
	      (ltk:focus search-entry)))
      (show-projects-in-listbox (projects) search-listbox)

      (ltk:pack search-entry :expand 1 :fill :both :padx 5 :pady 3)
      (ltk:pack frame :expand t :fill :both :padx 5 :pady 3)
      (ltk:pack search-listbox :side :left :expand t :fill :both))))