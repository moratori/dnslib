(in-package :cl-user)
(defpackage dnslib.core.encoder
  (:use :cl
        :dnslib.core.types
        :dnslib.core.errors
        :dnslib.core.util
        )
  (:export 
    :encode
    )
  (:documentation
    "RR構造体のrdata部分の構造体をパースしubyte-array 8の値に変換する"))
(in-package :dnslib.core.encoder)


(defmethod encode ((type (eql 2)) rdata)
  (let* ((oct 
           (to-ubyte8-array rdata))
         (size 
           (name-len oct))
         (res 
           (make-array 
             (list size)
             :element-type '(unsigned-byte 8)
             :initial-element 0)))

    (declare (type (simple-array (unsigned-byte 8)) res))

    (set-name oct res 0)

    res))

(defmethod encode ((type (eql 5)) rdata)
  (encode 2 rdata))

(defmethod encode ((type (eql 16)) rdata)

  (let ((res 
          (make-array 
            (list (length rdata))
            :element-type '(unsigned-byte 8)
            :initial-element 0)))
    (declare (type (simple-array (unsigned-byte 8)) res))

    (loop 
      with i = 0
      for ch across rdata
      do 
      (setf (aref res i) (char-code ch))
      (incf i))
    res))

(defmethod encode ((type (eql 28)) rdata)
  (let ((res 
          (make-array 
            (list 16)
            :element-type '(unsigned-byte 8)
            :initial-element 0)))
    (loop 
      for i from 0 by 2
      for oct in rdata do
      (multiple-value-bind 
        (value pos) (parse-integer oct :radix 16 :junk-allowed nil)
        (set-upper8 value res i)
        (set-lower8 value res (1+ i))))

    res))

(defmethod encode ((type number) rdata)
  rdata)


