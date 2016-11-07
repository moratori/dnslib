(in-package :cl-user)
(defpackage dnslib.core.rdataparser
  (:use :cl
        :dnslib.core.types
        :dnslib.core.rtypes
        :dnslib.core.errors
        )
  (:export 
    :parserdata))

(in-package :dnslib.core.rdataparser)


(defvar 
  *typedef*
  (make-hash-table :test 'eq))

(setf (gethash 1 *typedef*) #'a
      (gethash 28 *typedef*) #'aaaa
      )

(defun parserdata (ubyte-array len rr)
  
  (multiple-value-bind 
    (constructor flag)
    (gethash (rr.type rr) *typedef*)
    (unless flag 
      (error "unimplemented type"))
    (let ((result (funcall constructor)))
      (%parserdata result rr ubyte-array len)
      result
      )
    )
  )



(defmethod %parserdata ((result a) (rr rr) ubyte-array len)
  
  )





