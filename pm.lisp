(ql:quickload "cl-fad")
(ql:quickload "split-sequence")

(defparameter *projects-path* #P"~/Projekte/")
(defparameter *project-directory-parts* '(:namespace :technology :year :name))
(defparameter *path-exports* '(#P"/usr/local/bin/"))

(defvar *project-functions* nil)
(defvar *projects* nil)

(load "macros.lisp")
(load "make-project.lisp")
(load "get-project.lisp")
(load "project-functions.lisp")