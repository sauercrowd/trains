(in-package :trains)

(defconstant *allowed-html-tags* '(
                                    "a"
                                    "abbr"
                                    "acronym"
                                    "address"
                                    "animate"
                                    "animatemotion"
                                    "animatetransform"
                                    "applet"
                                    "area"
                                    "article"
                                    "aside"
                                    "audio"
                                    "b"
                                    "base"
                                    "basefont"
                                    "bdi"
                                    "bdo"
                                    "bgsound"
                                    "big"
                                    "blink"
                                    "blockquote"
                                    "body"
                                    "br"
                                    "button"
                                    "canvas"
                                    "caption"
                                    "center"
                                    "cite"
                                    "code"
                                    "col"
                                    "colgroup"
                                    "command"
                                    "content"
                                    "custom tags"
                                    "data"
                                    "datalist"
                                    "dd"
                                    "del"
                                    "details"
                                    "dfn"
                                    "dialog"
                                    "dir"
                                    "discard"
                                    "div"
                                    "dl"
                                    "dt"
                                    "element"
                                    "em"
                                    "embed"
                                    "fieldset"
                                    "figcaption"
                                    "figure"
                                    "font"
                                    "footer"
                                    "form"
                                    "frame"
                                    "frameset"
                                    "h1"
                                    "h2"
                                    "h3"
                                    "h4"
                                    "h5"
                                    "h6"
                                    "head"
                                    "header"
                                    "hgroup"
                                    "hr"
                                    "html"
                                    "i"
                                    "iframe"
                                    "image"
                                    "image2"
                                    "image3"
                                    "img"
                                    "img2"
                                    "input"
                                    "input2"
                                    "input3"
                                    "input4"
                                    "ins"
                                    "isindex"
                                    "kbd"
                                    "keygen"
                                    "label"
                                    "legend"
                                    "li"
                                    "link"
                                    "listing"
                                    "main"
                                    "map"
                                    "mark"
                                    "marquee"
                                    "menu"
                                    "menuitem"
                                    "meta"
                                    "meter"
                                    "multicol"
                                    "nav"
                                    "nextid"
                                    "nobr"
                                    "noembed"
                                    "noframes"
                                    "noscript"
                                    "object"
                                    "ol"
                                    "optgroup"
                                    "option"
                                    "output"
                                    "p"
                                    "param"
                                    "portal"
                                    "picture"
                                    "plaintext"
                                    "pre"
                                    "progress"
                                    "q"
                                    "rb"
                                    "rp"
                                    "rt"
                                    "rtc"
                                    "ruby"
                                    "s"
                                    "samp"
                                    "script"
                                    "search"
                                    "section"
                                    "select"
                                    "set"
                                    "shadow"
                                    "slot"
                                    "small"
                                    "source"
                                    "spacer"
                                    "span"
                                    "strike"
                                    "strong"
                                    "style"
                                    "sub"
                                    "summary"
                                    "sup"
                                    "svg"
                                    "table"
                                    "tbody"
                                    "td"
                                    "template"
                                    "textarea"
                                    "tfoot"
                                    "th"
                                    "thead"
                                    "time"
                                    "title"
                                    "tr"
                                    "track"
                                    "tt"
                                    "u"
                                    "ul"
                                    "var"
                                    "video"
                                    "wbr"
                                    "xmp"))
(defun content-to-string (content)
  (if (stringp content)
      content
      (format nil "~{~A~^ ~}" content)))

(defun tag-key-to-string (k)
  (if (stringp k)
      k
      (string-downcase (symbol-name k))))


(defun create-tags (args)
  (let* ((k (car args))
         (remaining-list (cdr args))
         (v (car remaining-list)))
    (if v
        (format NIL "~a=\"~a\" ~a" (tag-key-to-string k) v (create-tags
                                                            (cdr remaining-list)))
        "")))


(defmacro register-html-function (tagname)
  (let ((fn-name (read-from-string (format nil "h-~A" tagname))))
    `(progn (defun ,fn-name (&rest args)
              (if (stringp (car args))
                  (format nil "<~a>~a</~a>" ,tagname (content-to-string args)  ,tagname)
                  (format nil "<~a ~a>~a</~a>" ,tagname (create-tags (car args)) (content-to-string (cdr args))  ,tagname))
              )
            (export ',fn-name))))


(register-html-function "div")

(defmacro create-html-tags (tag-list)
  (print (type-of (eval tag-list)))
  `(progn
     ,@(loop for tag in (eval tag-list)
             collect `(register-html-function ,tag))))


(create-html-tags *allowed-html-tags*)

(defun html-tag-p (tag)
  (member (tag-key-to-string tag) *allowed-html-tags* :test #'equal))
