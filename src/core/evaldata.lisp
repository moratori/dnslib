(in-package :cl-user)
(defpackage dnslib.core.evaldata
  (:use :cl
        :dnslib.core.types
        :dnslib.core.errors
        :dnslib.core.util
        )
  (:export 
    :evaldata-top
    )
  (:documentation
    "RR構造体のrdata部分のバイト配列をパースし解釈する"))
(in-package :dnslib.core.evaldata)




(defun evaldata-top (rr ubyte-array)
  "rdataを評価するトップレベル関数"
  
  (evaldata (rr.type rr) rr ubyte-array (length ubyte-array)))



(defmethod evaldata ((type (eql 2)) (rr rr) ubyte-array len)
  "NSレコードのRRについてRDATAをパースする"

  (let* ((rdata (rr.rdata rr))
         (mylen (+ 12 (rr.rdlength rr)))
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
            (a b tmp) (%parse-name ubyte-array len start)
            (declare (ignore _))
            (append result tmp))
          result))))


(defmethod evaldata ((type (eql 5)) (rr rr) ubyte-array len)
  "CNAMEレコードのRRについてRDATAをパースする"

  (evaldata 2 rr ubyte-array len))


