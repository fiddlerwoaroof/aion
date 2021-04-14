;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Package: ASDF-USER -*-
(in-package :asdf-user)

(defsystem :aion
  :description ""
  :author "Ed L <edward@elangley.org>"
  :license "MIT"
  :depends-on (:alexandria
               :uiop
               :serapeum
               :fwoar-lisputils
               :data-lens)
  :serial t
  :components ((:file "packages")
               (:file "parser")
               (:file "build-tree")))
(defsystem :aion/sqlite
  :description ""
  :author "Ed L <edward@elangley.org>"
  :license "MIT"
  :depends-on (:aion
               :alexandria
               :cl-ppcre
               :fwoar-lisputils
               :local-time
               :serapeum
               :sqlite
               :sxql
               :trivia)
  :serial t
  :components ((:file "sqlite")))
