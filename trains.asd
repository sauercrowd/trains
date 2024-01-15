(asdf:defsystem "trains"
  :depends-on (:clack :hunchentoot :asdf)
  :components ((:module "src"
                        :components
                        ((:file "package")
                         (:file "html")
                         (:file "views")
                         (:file "server")))))


