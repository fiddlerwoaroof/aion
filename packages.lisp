(defpackage :aion.packages
  (:use :cl )
  (:export ))
(in-package :aion.packages)

(defpackage :aion.parser
  (:use :cl)
  (:export #:handle-begin
           #:handle-end
           #:handle-property
           #:process-ics))

(defpackage :aion.build-tree
  (:use :cl)
  (:import-from #:aion.parser #:handle-begin #:handle-end
                #:handle-property #:process-ics)
  (:export #:ics->tree))

(uiop:define-package :aion
    (:use)
  (:import-from :aion.parser #:handle-begin #:handle-end #:handle-property)
  (:import-from :aion.build-tree #:ics->tree)
  (:reexport :aion.build-tree :aion.parser))
