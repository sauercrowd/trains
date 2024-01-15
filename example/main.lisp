(defpackage :trains-example
            (:use :cl :trains)
            (:export :main-fn))

(declaim (optimize (debug 3) (safety 3) (speed 0)))

(in-package :trains-example)

(trains:register-app-directory (asdf:system-relative-pathname :trains-example "app"))


(defun main-fn ()
  (trains:start-http-server
    :package :trains-example
    :port 8080
    :mode :dev))
