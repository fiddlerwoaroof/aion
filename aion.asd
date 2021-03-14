;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Package: ASDF-USER -*-
(in-package :asdf-user)

(defsystem :aion
  :description ""
  :author "Ed L <edward@elangley.org>"
  :license "MIT"
  :depends-on (:alexandria
               :uiop
               :serapeum
               :fwoar-lisputils)
  :serial t
  :components ((:file "packages")
               (:file "parser")
               (:file "build-tree")))
