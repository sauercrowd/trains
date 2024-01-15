(in-package :trains)

(defparameter *global-route-table* '())

(defparameter *action-to-handler* (make-hash-table :test #'equal))

(defmacro mount-dispatcher ()
  '(defun dispatch-action (controller action args)
     (let ((fn (find-symbol (format nil "~A~~~A"
                                    (string controller)
                                    (string action)))))
       (if fn
           (funcall fn args)
           (not-found-response)))))


(defun get-route-candidates (request-method request-route)
  (loop for route-candidate in *global-route-table*
        collect
        (destructuring-bind (method route-candidate-parts controller action) route-candidate
          (if (string= request-method method)
              (let ((params (check-route
                              (cl-ppcre:split "/" request-route)
                              route-candidate-parts
                              (make-hash-table))))
                (if params
                    (progn
                      (maphash (lambda (key value)
                                 (setf (gethash key *call-context* ) value)) params)
                      (list controller action params))
                    nil))
              nil))))




(defun check-route (route-parts route-candidate params)
  (format t "~A ~A ~A~%" (car route-parts) (car route-candidate) params)
  (if (and (null route-parts)
           (null route-candidate))
          params
          (let ((current-route-part (car route-parts))
                (current-route-candidate-part (car route-candidate)))
            (cond (
                   (and
                     (< 0 (length current-route-candidate-part))
                     (char= #\: (char current-route-candidate-part 0)))
                   (setf (gethash current-route-candidate-part params) current-route-part)
                   (check-route (cdr route-parts) (cdr route-candidate) params))
                  (
                   (string= current-route-part current-route-candidate-part)
                   (check-route (cdr route-parts) (cdr route-candidate) params))
                  (t nil)))))


(defun add-route (method route-path controller action)
  `(setq *global-route-table* (append *global-route-table*
                                      '((,method ,(cl-ppcre:split "/" route-path) ,controller ,action)))))

(defmacro route (method prefix controller action)
  (if (equal method :resources)
  `(progn
         ,(add-route :get prefix controller :index)
         ,(add-route :get (format nil "~A/new" prefix) controller :new)
         ,(add-route :post prefix controller :create)
         ,(add-route :get (format nil "~A/:id" prefix) controller :show)
         ,(add-route :get (format nil "~A/:id/edit" prefix) controller :edit)
         ,(add-route :patch (format nil "~A/:id" prefix) controller :update)
         ,(add-route :put (format nil "~A/:id" prefix) controller :update)
         ,(add-route :delete (format nil "~A/:id" prefix) controller :destroy))
      (add-route method prefix controller action)))



(defvar *call-context* nil)

(defun @= (name value)
  (setf (gethash name *call-context*) value))

(defun <% (name)
  (format t "call-context ~A~%" *call-context*)
  (if *call-context*
       (or (gethash name *call-context*) "")
       ""))


(defclass train-context ()
  ((package :initarg :package :accessor context-package)
   (mode :initarg :mode :accessor context-mode)))


(defmacro with-controller (controller-name &body body)
  `(progn
     ,@(loop for body-element in body
             collect
             (if (equal 'defun (car body-element))
                 (let ((fn-name (intern
                                  (string-upcase
                                    (format nil "~A~~~A"
                                            (string controller-name)
                                            (string
                                              (second body-element)))))))
                   `(progn
                      (defun ,fn-name
                        ,@(subseq body-element 2))
                      (export ',fn-name)))))))



(defun not-found-response ()
  '(404 (:content-type "text/plain") ("not found")))

(defun handle-request (info context)
    (if info
        (destructuring-bind (controller action params) info
          (let  ((maybe-fn
                   (find-symbol (string-upcase (format nil "~A~~~A" (string controller)
                                                       (string action)))
                                (context-package context))))
            (if maybe-fn
                (process-controller-response
                  (funcall maybe-fn "")
                  (context-package context)
                  controller
                  action)
                (not-found-response))))
        (not-found-response)))

(defparameter *app-directory* nil)

(defun process-controller-response (result package controller action)
  (if (and
        (listp result)
        (equal (length result) 3))
      (let ((status-code (first result))
            (headers (second result))
            (body (third result)))
        (list status-code headers (if (listp body) body (list body))))
       (render-view *app-directory* package controller action)))



(defun register-app-directory (app-directory-path)
  (setf *app-directory* app-directory-path)
  (let ((app-path (concatenate 'string (namestring app-directory-path) "/")))
    (load (merge-pathnames app-path "routes.lisp"))
    (dolist (file (directory
                    (merge-pathnames (format nil "~A/controllers/" (string app-path) ) "*.lisp")))
            (load file))))


(defun handler (env context)
  (let ((request-route (getf env :path-info))
        (*call-context* (make-hash-table :test #'equal))
        (request-method (getf env :request-method)))
    (handle-request
      (car
        (remove-if #'null
                   (get-route-candidates request-method request-route)))
      context)))


(defparameter *internal-dev-mode* t)

(defun start-http-server (&key package mode (port 8078) (address "127.0.0.1"))
  (defparameter *clack-app*
    (clack:clackup #'(lambda (env)
                       (handler env
                                (make-instance 'train-context :package package
                                               :mode mode)))
                   :address address
                   :port port
                   :use-thread *internal-dev-mode*)))
