(defpackage :aion.sqlite
  (:use :cl )
  (:import-from :aion.parser
                #:handle-begin #:handle-end
                #:handle-property #:process-ics)
  (:export
   #:ics->sqlite))
(in-package :aion.sqlite)

(fw.lu:defclass+ emit-sql ()
  ((%lines :accessor lines :initform ())
   (%tzid :accessor tzid :initform nil)
   (%db :reader db :initarg :db)))
(defmethod handle-begin ((client emit-sql) block)
  (values))
(defmethod handle-end :after ((client emit-sql) block)
  (setf (lines client) nil))
(defmethod handle-end ((client emit-sql) block)
  )
(defmethod handle-end :before ((client emit-sql) block)
  (values))
(defmethod handle-end ((client emit-sql) (block (eql :vevent)))
  (macrolet ((get-setter ()
               `(sxql:set= ,@(mapcan (lambda (it)
                                       (list it `(serapeum:assocadr
                                                  ,(alexandria:make-keyword
                                                    (substitute #\- #\_
                                                                (string it)))
                                                  (lines client))))
                                     '(:X_APPLE_TRAVEL_ADVISORY_BEHAVIOR
                                       :VEVENT :VALARM :RECURRENCE_ID
                                       :ORGANIZER :LAST_MODIFIED
                                       :EXDATE :CREATED :ATTENDEE
                                       :ATTENDEE :ATTACH :CATEGORIES
                                       :DESCRIPTION :DTEND :DTSTAMP
                                       :DTSTART :GEO :LOCATION :RRULE
                                       :SEQUENCE :STATUS :SUMMARY
                                       :TRANSP :UID :URL
                                       :X_ALT_DESC)))))
    (multiple-value-bind (query params)
        (sxql:yield
         (sxql:insert-into :vevent
           (get-setter)))
      (apply 'sqlite:execute-single
             (db client)
             query
             params))))
(defmethod handle-property ((client emit-sql) tag params content)
  (push (list tag content)
        (lines client)))

(defparameter +datetime-scanner+
  (cl-ppcre:create-scanner
   '(:SEQUENCE
     :START-ANCHOR
     ;; date yyyy-mm-dd
     (:REGISTER (:GREEDY-REPETITION 4 4 :DIGIT-CLASS))
     (:REGISTER (:GREEDY-REPETITION 2 2 :DIGIT-CLASS))
     (:REGISTER (:GREEDY-REPETITION 2 2 :DIGIT-CLASS))
     #\T
     ;; time hh-mm-ss
     (:REGISTER (:GREEDY-REPETITION 2 2 :DIGIT-CLASS))
     (:REGISTER (:GREEDY-REPETITION 2 2 :DIGIT-CLASS))
     (:REGISTER (:GREEDY-REPETITION 2 2 :DIGIT-CLASS))
     ;; tz
     (:GREEDY-REPETITION 0 1 (:REGISTER #\Z))
     :END-ANCHOR)))
(defparameter +date-scanner+
  (cl-ppcre:create-scanner
   '(:SEQUENCE
     :START-ANCHOR
     ;; date yyyy-mm-dd
     (:REGISTER (:GREEDY-REPETITION 4 4 :DIGIT-CLASS))
     (:REGISTER (:GREEDY-REPETITION 2 2 :DIGIT-CLASS))
     (:REGISTER (:GREEDY-REPETITION 2 2 :DIGIT-CLASS))
     :END-ANCHOR)))
(defparameter +time-scanner+
  (cl-ppcre:create-scanner
   '(:SEQUENCE
     :START-ANCHOR
     ;; time hh-mm-ss
     (:REGISTER (:GREEDY-REPETITION 2 2 :DIGIT-CLASS))
     (:REGISTER (:GREEDY-REPETITION 2 2 :DIGIT-CLASS))
     (:REGISTER (:GREEDY-REPETITION 2 2 :DIGIT-CLASS))
     ;; tz
     (:GREEDY-REPETITION 0 1 (:REGISTER #\Z))
     :END-ANCHOR)))

(defparameter +sqlite-format+
  '((:YEAR 4) #\- (:MONTH 2) #\- (:DAY 2)
    #\space (:HOUR 2) #\: (:MIN 2) #\: (:SEC 2)
    :GMT-OFFSET-OR-Z))

(defun parse-datetime (time timezone)
  (trivia:ematch (nth-value 1 (cl-ppcre:scan-to-strings +datetime-scanner+ time))
    (#(ye mo da ho mi se tz)
      (local-time:encode-timestamp
       0
       (fw.lu:if-let* ((se (parse-integer se))
                       (_ (= 60 se)))
         59
         se)
       (parse-integer mi) (parse-integer ho)
       (parse-integer da) (parse-integer mo) (parse-integer ye)
       :timezone (if (equal tz "Z")
                     local-time:+utc-zone+
                     timezone)))))

(defun parse-date (time)
  (trivia:ematch (nth-value 1 (cl-ppcre:scan-to-strings +date-scanner+ time))
    (#(ye mo da)
      (local-time:encode-timestamp
       0 0 0 0
       (parse-integer da) (parse-integer mo) (parse-integer ye)))))

;;what do I do for the date here???
#+(or)
(defun parse-time (time)
  (trivia::match (nth-value 1 (cl-ppcre:scan-to-strings +time-scanner+ time))
    (#(ho mi se)
      (local-time:make-timestamp
       0 0 0 0
       (parse-integer da) (parse-integer mo) (parse-integer ye)))))

(defun handle-ical-date (client tag params content)
  (push (list tag
              (local-time:format-timestring
               nil
               (case (alexandria:make-keyword
                      (string-upcase
                       (serapeum:assocadr :value params)))
                 (:date (parse-date content))
                 (t (parse-datetime content
                                    (or (local-time:find-timezone-by-location-name
                                         (serapeum:assocadr :tzid params))
                                        (tzid client)))))
               :format +sqlite-format+))
        (lines client)))

(defmethod handle-property ((client emit-sql) (tag (eql :dtstart)) params content)
  (handle-ical-date client tag params content))
(defmethod handle-property ((client emit-sql) (tag (eql :dtend)) params content)
  (handle-ical-date client tag params content))
(defmethod handle-property ((client emit-sql) (tag (eql :created)) params content)
  (handle-ical-date client tag params content))
(defmethod handle-property ((client emit-sql) (tag (eql :dtstamp)) params content)
  (handle-ical-date client tag params content))

(defun setup-sql ()
  (sxql:yield
   (sxql:create-table (:vevent :if-not-exists t)
       ((:ATTACH :type 'text)
        (:ATTENDEE :type 'text)
        (:CATEGORIES :type 'text)
        (:CREATED :type 'text)
        (:DESCRIPTION :type 'text)
        (:DTEND :type 'text)
        (:DTSTAMP :type 'text)
        (:DTSTART :type 'text)
        (:EXDATE :type 'text)
        (:GEO :type 'text)
        (:LAST_MODIFIED :type 'text)
        (:LOCATION :type 'text)
        (:ORGANIZER :type 'text)
        (:RECURRENCE_ID :type 'text)
        (:RRULE :type 'text)
        (:SEQUENCE :type 'text)
        (:STATUS :type 'text)
        (:SUMMARY :type 'text)
        (:TRANSP :type 'text)
        (:UID :type 'text)
        (:URL :type 'text)
        (:VALARM :type 'text)
        (:VEVENT :type 'text)
        (:X_ALT_DESC :type 'text)
        (:X_APPLE_TRAVEL_ADVISORY_BEHAVIOR :type 'text))
     (sxql:primary-key '(:sequence :uid :recurrence_id)))))

(defun ics->sqlite (fn data)
  (sqlite:with-open-database (db fn)
    (sqlite:execute-non-query
     db (sxql:yield (sxql:drop-table :vevent :if-exists t)))
    (sqlite:execute-non-query
     db (setup-sql))
    (sqlite:with-transaction db
      (process-ics (emit-sql db) data))))
