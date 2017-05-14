(in-package :cl-user)
(defpackage dnslib.core.decoder
  (:use :cl
        :dnslib.core.types
        :dnslib.core.errors
        :dnslib.core.util
        )
  (:export 
    :decode
    )
  (:documentation
    "RR構造体のrdata部分のバイト配列をパースし解釈する"))
(in-package :dnslib.core.decoder)


(defmethod decode ((type (eql 2)) rdata rdata-len ubyte-array ubyte-array-len)
  "NSレコードのRRについてRDATAをパースする"

  (let* ((mylen (+ 12 rdata-len))
         (con   (make-array (list mylen) :element-type '(unsigned-byte 8)
                                         :initial-element 0)))

    (loop for val being the elements of rdata
          for i from 0
          do (setf (aref con (+ i 12)) val))

    (multiple-value-bind 
      (continue-flag start result)
      (%parse-name con mylen 12 :evalstep t)

      (if continue-flag 
          (multiple-value-bind 
            (a b tmp) (%parse-name ubyte-array ubyte-array-len start)
            (declare (ignore a b))
            (append result tmp))
          result))))


(defmethod decode ((type (eql 5)) rdata rdata-len ubyte-array ubyte-array-len)
  "CNAMEレコードのRRについてRDATAをパースする"

  (decode 2 rdata rdata-len ubyte-array ubyte-array-len))


(defmethod decode ((type (eql 16)) rdata rdata-len ubyte-array ubyte-array-len)
  "TXTレコードのRRについてRDATAをパースする"

  (concatenate 'string 
               (loop 
                 for code across rdata 
                 collect (code-char code))))

(defmethod decode ((type (eql 28)) rdata rdata-len ubyte-array ubyte-array-len)
  "AAAAレコードのRRについてRDATAをパースする"

    (unless (= (length rdata) 16)
      (error "something error"))

    (loop 
      for i from 0 upto 14 by 2
      collect (format nil "~x" (concat-byte (aref rdata i) (aref rdata (1+ i))))))





(defmethod decode ((type (eql 6)) rdata rdata-len ubyte-array ubyte-array-len)
  "SOAレコードのRRについてRDATAをパースする"

  rdata
  )


(defmethod decode ((type (eql 46)) rdata rdata-len ubyte-array ubyte-array-len)
  "RRSIGレコードのRRについてRDATAをパースする"

  rdata
  )


(defmethod decode ((type (eql 47)) rdata rdata-len ubyte-array ubyte-array-len)
  "NSECレコードのRRについてRDATAをパースする"

  rdata
  )


(defmethod decode ((type (eql 48)) rdata rdata-len ubyte-array ubyte-array-len)
  "DNSKEYレコードのRRについてRDATAをパースする"

  rdata
  )




(defmethod decode ((type number) rdata rdata-len ubyte-array ubyte-array-len)
  (warn 
    (make-condition 
      'data-decode-error
      :data type))
  rdata)





