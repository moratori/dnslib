(in-package :cl-user)
(defpackage dnslib.core.errors
  (:use :cl
        )
  (:export 
    :qp-error
    :qt-error
    :ql-error
    :dp-error
    :mq-error
    :aaa-error
    )
  )
(in-package :dnslib.core.errors)



(defun qp-error (data mes)
  (raise-condition 'qname-pointer-error data mes))

(defun ql-error (data mes)
  (raise-condition 'qname-len-error data mes))

(defun qt-error (data mes)
  (raise-condition 'qname-terminal-error data mes))

(defun dp-error (data mes)
  (raise-condition 'data-parse-error data mes))

(defun mq-error (data mes)
  (raise-condition 'malformed-question-error data mes))
 
(defun aaa-error (data mes)
  (raise-condition 'malformed-aaa-section-error data mes))
 


(defun raise-condition (condname data mes)
  (error 
    (make-condition condname :data data :mes mes)))



(define-condition data-parse-error (error)
  ((data
     :initform nil
     :initarg :raw)
   (mes 
     :initform "parse error"
     :initarg :mes)))


(define-condition qname-pointer-error (data-parse-error)
  ()
  )

(define-condition qname-len-error (data-parse-error)
  ()
  )

(define-condition qname-terminal-error (data-parse-error)
  ()
  )

(define-condition malformed-question-error (data-parse-error)
  ()
  )

(define-condition malformed-aaa-section-error (data-parse-error)
  ()
  )

