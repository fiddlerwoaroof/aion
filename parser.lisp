(in-package :aion.parser)

(defgeneric handle-begin (client block)
  (:documentation "handle the beginning of a new block in the iCalendar data"))
(defgeneric handle-end (client block)
  (:documentation "handle the ending of a block in the iCalendar data"))
(defgeneric handle-property (client tag params content)
  (:documentation "handle a property for the current iCalendar block"))

(defun get-line (stream)
  (loop for line = (read-line stream nil)
        while line
        collect line into results
        while (eql #\space (peek-char nil stream nil))
        finally (return (when results
                          (string-right-trim
                           '(#\newline #\return)
                           (serapeum:string-replace-all
                            #1=#.(coerce (list #\return #\space)
                                         'string)
                            (serapeum:string-join results "")
                            ""))))))

(defmacro with-temporary-keywords ((intern) &body body)
  (alexandria:with-gensyms (kw-list)
    `(let ((,kw-list '()))
       (unwind-protect
            (flet ((,intern (inp)
                     (multiple-value-bind (kw existing?)
                         (alexandria:make-keyword (string-upcase inp))
                       (prog1 kw
                         (unless existing?
                           (push kw ,kw-list))))))
              ,@body)
         (mapc 'unintern ,kw-list)))))

(defgeneric as-stream (it)
  (:method ((it string))
    (make-string-input-stream it))
  (:method ((it pathname))
    (open it))
  (:method ((it stream))
    it))

(defun process-ics (client file)
  (let ((states '()))
    (with-temporary-keywords (normalize)
      (labels ((%handle-block-delimiter (tag type)
                 (push type states)
                 (ecase tag
                   ((:begin) (handle-begin client type))
                   ((:end) (handle-end client type))))
               (parse-params (inp)
                 (destructuring-bind (head params) (fwoar.string-utils:partition #\; inp)
                   (values head
                           (when params
                             (map 'list
                                  (data-lens:• (data-lens:transform-head #'normalize)
                                               (serapeum:op
                                                 (fwoar.string-utils:partition #\= _)))
                                  (fwoar.string-utils:split #\; params))))))
               (parse-property (it)
                 (destructuring-bind (s e) (fwoar.string-utils:partition #\: it)
                   (multiple-value-bind (head params) (parse-params s)
                     (list (normalize head)
                           params
                           e))))
               (%handle-property (it)
                 (apply 'handle-property client it))
               (handle-line (tag tagged line)
                 (case tag
                   ((:begin)
                    (%handle-block-delimiter tag (normalize tagged)))
                   ((:end)
                    (%handle-block-delimiter tag (normalize tagged)))
                   (t (%handle-property (parse-property line))))))
        (with-open-stream (s (as-stream file))
          (loop for line = (get-line s)
                for (tag tagged) = (if line
                                       (fwoar.string-utils:partition #\: line)
                                       '(nil nil))
                while line
                do (handle-line (fw.lu:may (normalize tag))
                                tagged
                                line)))))))
