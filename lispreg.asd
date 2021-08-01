;;;; This file contains the system definition for ASDF

(in-package :asdf-user)

(defsystem #:lreg
  :description "My own implementation of regular expression system"
  :version "0.0.1"
  :author "bruhulance"
  :licence "MIT"
  :serial t
  :components
  ((:file "src/package")
   (:file "src/utils")))
