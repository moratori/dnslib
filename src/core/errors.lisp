(in-package :cl-user)
(defpackage dnslib.core.errors
  (:use :cl
        )
  (:export 
    :mes
    :data

    :data-parse-error
    
    :qp-error
    :qt-error
    :ql-error
    :dp-error
    :mq-error
    :aaa-error

    :data-decode-error
    :data-encode-error
    
    )
  )
(in-package :dnslib.core.errors)


(defun raise-condition (condname data mes)
  (error 
    (make-condition condname :data data :mes mes)))

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




(define-condition data-parse-error (error)
  ((data
     :accessor data
     :initform nil
     :initarg :data)
   (mes 
     :accessor mes
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





(define-condition data-encode-error (error)
  ((mes 
     :accessor mes
     :initform ""
     :initarg :mes)))



(define-condition data-decode-error (warning)
  ((mes 
     :accessor mes
     :initform "unimplemented resource record type while decoding RR"
     :initarg :mes)))










