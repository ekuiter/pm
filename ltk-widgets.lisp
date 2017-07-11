(in-package :ltk-widgets)

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
      (let ((ltk-type (intern (symbol-name type) 'ltk)))
	(when (equal ltk-type 'ltk:frame)
	  (setf pack (append pack '(:fill :both :anchor :nw))))
	`(let ((,instance-sym (make-instance ',ltk-type :master ,parent ,@init)))
	   ,@(loop while configure collect
		  `(ltk:configure ,instance-sym ,(pop configure) ,(pop configure)))
	   ,(if grid
		`(ltk:grid ,instance-sym ,@grid)
		`(ltk:pack ,instance-sym ,@pack))
	   ,instance-sym)))))

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
