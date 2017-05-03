(in-package :cl-user)
(defpackage dnslib.core.util
  (:use :cl
        :dnslib.core.types
        :dnslib.core.errors
        )
  (:export 
    :concat-byte
    :concat-short
    :code-to-string
    :summation
    :set-upper8
    :set-lower8
    )
  )
(in-package :dnslib.core.util)


(defun summation (list fn)
  (reduce 
    (lambda (r x)
      (+ r (funcall fn x)))
    list 
    :initial-value 0))


(defun code-to-string (codes)
  "キャラクタコードの配列のリストを文字列のリストに変換する"
  (loop for arr in codes
        collect 
        (coerce 
          (mapcar #'code-char (coerce arr 'list))
          'string)))


(defun set-upper8 (n arr start)
  "上位1byteをarrのstartにセットする"

  (setf (aref arr start) (ash n -8))
  (1+ start))

(defun set-lower8 (n arr start)
  "下位1byteをarrのstartにセットする"

  (setf (aref arr start) (logand n 255))
  (1+ start))



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
 
