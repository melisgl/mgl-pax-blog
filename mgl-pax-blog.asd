;;;; -*- mode: Lisp -*-

(asdf:defsystem #:mgl-pax-blog
  :licence "MIT, see COPYING."
  :version "0.0.1"
  :author "Gábor Melis"
  :mailto "mega@retes.hu"
  :homepage "http://quotenil.com"
  :description "Static blog generator based on MGL-PAX."
  :depends-on (#:alexandria #:local-time #:mgl-pax/full
               #:pythonic-string-reader #:spinneret #:xml-emitter)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "blog")))))
