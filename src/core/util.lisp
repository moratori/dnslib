(in-package :cl-user)
(defpackage dnslib.core.util
  (:use :cl
        :dnslib.core.types
        :dnslib.core.errors
        )
  (:export 
    :concat-byte
    :concat-short
    )
  )
(in-package :dnslib.core.util)



(defun concat-byte (msb lsb)
  "msbを上位８bit、lsbを下位8bitとする
   16bitの数を返す"
  
  (declare (type fixnum msb lsb))
  
  (logior (ash msb 8) lsb))


(defun concat-short (msb lsb)
  "msbを上位16bit、lsbを下位16bitとする
   32bitの数を返す"

  (declare (type fixnum msb lsb))
  
  (logior (ash msb 16) lsb))
 
