;;;; This file contains the system definition for ASDF

(in-package :asdf-user)

(defsystem #:lreg
  :description "My own implementation of regular expression system"
  :version "0.2.0"
  :author "bruhulance"
  :licence "MIT"
  :serial t
  :components
  ((:file "src/package")
   (:file "src/utils")))
