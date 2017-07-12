(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "cl-fad")
  (ql:quickload "split-sequence")
  (load "ltk/ltk"))

(defpackage :project
  (:use :common-lisp)
  (:export :namespace :technology :year :name :path :all-projects :show-projects
	   :find-projects :search-projects :find-project :search-project
	   :where :show :project-directory :run :open-in-finder :open-in-terminal
	   :open-in-aquamacs :gitp :git-run :git-current-branch :remotes :git-pull
	   :git-push :git-stats :git-details :*empty-project* :emptyp))

(defpackage :ltk-widgets
  (:use :common-lisp)
  (:export :require-package :configure-style :center-window :draw-image
	   :move-listbox-selection :set-label-text :let-widgets :command
	   :bind :details-grid :evt))

(defpackage :gui
  (:use :common-lisp :project :ltk-widgets)
  (:export :gui))

(load "project")
(load "ltk-widgets")
(load "gui")
