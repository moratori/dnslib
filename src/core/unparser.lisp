(in-package :cl-user)
(defpackage dnslib.core.unparser
  (:use :cl
        :dnslib.core.types
        :dnslib.core.errors
        )
  (:export 
    :unparse-direct
    :sizeof-direct

    :unparse-compress
    :sizeof-compress
    )
  )
(in-package :dnslib.core.unparser)


#|
  nameの圧縮部分の実装でいい方法を思いつかないので
  圧縮なしの配列を返す。
  所定のサイズ(512を意図)を超えてしまう場合には
  TCフラグを立てるなどの対応が必要
|#


(defun %sum (list fn)
  (reduce 
    (lambda (r x)
      (+ r (funcall fn x)))
    list 
    :initial-value 0))

(defun name-len (name)
  "nameは配列のリスト
   unsigned-byte 8な配列に変換するに当たって何バイト使うか
   計算して返す"
  
  (+ 1 
     (length name)
     (loop for each in name sum (length each))))


(defmethod size-of-direct ((header header))
  "dnsヘッダのサイズを返す"

  (declare (ignore header))

  12)

(defmethod size-of-direct ((question question))
  "dnsのquestionセクションのサイズを返す"
  
  (let ((qname (question.qname question)))
    (+ 4 (name-len qname))))

(defmethod size-of-direct ((rr rr))
  "resourceレコードのサイズを返す"
  
  (+ 
    (name-len (rr.name rr))
    2 2 4 2 (rr.rdlength rr)))

(defmethod sizeof-direct ((dns dns))
  "DNS構造体をnameの圧縮なしに配列に変換した場合
   の配列のサイズを返す"

  (declare (type dns dns))

  (let ((fn #'size-of-direct)
        (targets '(#'dns.question #'dns.answer #'dns.authority #'additional)))
    (+ 
      (sizeof-direct (dns.header dns))
      (loop for tfn in targets sum 
            (%sum (funcall tfn dns) fn)))))



(defmethod unparse ((rr rr) arr start)
  "arrのstart以降にリソースレコードを入れる"
  
  )

(defmethod unparse ((question question) arr start)
  "arrのstart以降にquestionを入れる"

  
  )

(defmethod unparse ((header header) arr start)
  "arrのstart以降にheaderを入れる"


  12
  )



(defun %iterate-multiple-sections (sections arr start)
  "funcはunparse-questionかunparse-aaaのいづれか
   arrのstartから始まるところにsections(リスト)の要素を
   連続で変換して入れるためのラッパ"
  
  (let ((next start))
    (loop 
      for each in sections
      do (setf next (unparse each arr next)))
    next))


(defun unparse-direct (dns size)
  "DNS構造体からunsigned-byte 8 な配列を返す
   nameの圧縮は行わない
   ;; sizeは、size-of-directで計算された配列のサイズ"

  (declare (type dns dns))

  (when (< size 12)
      (dp-error dns "malformed dns header(too short)"))
  
  (let ((result 
          (make-array 
            (list size)
            :element-type '(unsigned-byte 8)
            :initial-element 0))
        (start 0))
    
    (setf start (unparse (dns.header dns) result start)
          start (%iterate-multiple-sections (dns.question dns) result start)
          start (%iterate-multiple-sections (dns.answer dns) result start)
          start (%iterate-multiple-sections (dns.authority dns) result start)
          start (%iterate-multiple-sections (dns.additional dns) result start))

    result))








(defun sizeof-compress (dns)
  "DNS構造体をnameの圧縮ありで配列に変換した場合
   の配列のサイズを返す"

  (declare (ignore dns))
  -1
  )


(defun unparse-compress (dns)
  "DNS構造体からunsigned-byte 8 な配列を返す
   nameの圧縮は行わない"
  
  )
