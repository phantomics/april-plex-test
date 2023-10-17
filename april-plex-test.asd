;;;; april-plex-test.asd

(asdf:defsystem #:april-plex-test
  :description "Test integration of April and Petalisp"
  :author "Andrew Sengul"
  :license  ""
  :version "0.0.1"
  :serial t
  :depends-on ("april" "petalisp")
  :components ((:file "setup")
               (:file "plex")))
