(in-package :trains)

(defun default-html-headers ()
  nil)

(defun render-view (app-path package controller action)
  (let ((*package* (find-package package)))
    (let ((view-contents (with-open-file (s
                                           (string-downcase
                                             (format nil "~A/views/~A/~A.lisp" app-path controller action)))
                           (read s))))
      (let ((source (patch-html-tags view-contents)))
        (list 200
              (default-html-headers)
              (list
                (let ((*package* (find-package package)))
                  (eval
                    source))))))))


(defun parse-and-load-view (controller-name path)
  (let ((action-name (pathname-name path))
        (view-contents (with-open-file (s path)
                                    (read s))))
    (patch-html-tags view-contents)))

(defun patch-html-tags (view-contents)
  (let ((view-contents-processed view-contents))
    (if (and (listp view-contents-processed)
             (not (null view-contents-processed)))
        (let ((fn-name (car view-contents-processed))
               (patch-content
                 (loop for elt in (cdr view-contents-processed)
                       collect (patch-html-tags elt))))
          (if (and
               (symbolp fn-name)
               (not (keywordp fn-name))
               (html-tag-p fn-name))
             (cons (read-from-string (format nil "h-~A" fn-name)) patch-content)
            (cons fn-name patch-content)))
        view-contents-processed)))
