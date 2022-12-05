;;;; -*- mode: Lisp -*-

;;; See MGL-PAX-BLOG:@MGL-PAX-BLOG-MANUAL for the user guide.
(asdf:defsystem #:mgl-pax-blog
  :licence "MIT, see COPYING."
  :version "0.0.1"
  :author "Gábor Melis"
  :mailto "mega@retes.hu"
  :homepage "http://quotenil.com"
  :description "Static blog generator based on MGL-PAX."
  :depends-on (#:alexandria #:cl-who #:local-time #:mgl-pax/full
                            #:pythonic-string-reader)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "blog")))))
