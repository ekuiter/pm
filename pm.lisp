(ql:quickload "cl-fad")
(ql:quickload "split-sequence")
(load "ltk/ltk")

(defparameter *projects-path* #P"~/Projekte/")
(defparameter *project-directory-parts* '(:namespace :technology :year :name))
(defparameter *path-exports* '(#P"/usr/local/bin/"))

(defvar *project-functions* nil)
(defvar *projects* nil)

(dotimes (i 2)
  (load "macros")
  (load "make-project")
  (load "get-project")
  (load "project-functions")
  (load "gui"))