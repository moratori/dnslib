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

(defun set-name (name arr start)
  "nameをarrのstartから入れて次のポインタを返す"

  (let ((next start))
    (loop 
      for label in name
      do 
      (setf (aref arr next) (length label))
      (incf next)
      (loop 
        for ch across label
        do
        (setf (aref arr next) ch)
        (incf next)))
    (setf (aref arr next) 0)
    (1+ next)))

(defun set-upper8 (n arr start)
  "上位1byteをarrのstartにセットする"

  (setf (aref arr start) (ash n -8))
  (1+ start))

(defun set-lower8 (n arr start)
  "下位1byteをarrのstartにセットする"

  (setf (aref arr start) (logand n 255))
  (1+ start))


(defmethod sizeof-direct ((header header))
  "dnsヘッダのサイズを返す"

  (declare (ignore header))

  12)

(defmethod sizeof-direct ((question question))
  "dnsのquestionセクションのサイズを返す"
  
  (let ((qname (question.qname question)))
    (+ 4 (name-len qname))))

(defmethod sizeof-direct ((rr rr))
  "resourceレコードのサイズを返す"
  
  (+ 
    (name-len (rr.name rr))
    2 2 4 2 (rr.rdlength rr)))

(defmethod sizeof-direct ((dns dns))
  "DNS構造体をnameの圧縮なしに配列に変換した場合
   の配列のサイズを返す"

  (declare (type dns dns))

  (let ((fn #'sizeof-direct)
        (targets (list #'dns.question #'dns.answer #'dns.authority #'dns.additional)))
    (+ 
      (sizeof-direct (dns.header dns))
      (loop for tfn in targets sum 
            (%sum (funcall tfn dns) fn)))))



(defmethod unparse ((rr rr) arr start)
  "arrのstart以降にリソースレコードを入れる"
  
  (let* ((name (rr.name rr))
         (type (rr.type rr))
         (class (rr.class rr))
         (ttl (rr.ttl rr))
         (rdlength (rr.rdlength rr))
         (rdata (rr.rdata rr))
         (next (set-name name arr start)))
    
    (set-upper8 type arr next)
    (set-lower8 type arr  (+ 1 next))

    (set-upper8 class arr (+ 2 next))
    (set-lower8 class arr (+ 3 next))

    (setf 
      (aref arr (+ 4 next)) (ash ttl -24)
      (aref arr (+ 5 next)) (logand (ash ttl -16) 255)
      (aref arr (+ 6 next)) (logand (ash ttl -8) 255)
      (aref arr (+ 7 next)) (logand ttl 255))

    (set-upper8 rdlength arr (+ 8 next))
    (set-lower8 rdlength arr (+ 9 next))

    (let ((i (+ next 10)))
      (loop 
        for elm across rdata
        do 
        (setf (aref arr i) elm)
        (incf i))
      i)))


(defmethod unparse ((question question) arr start)
  "arrのstart以降にquestionを入れる"
  
  (let* ((qname (question.qname question))
         (qtype (question.qtype question))
         (qclass (question.qclass question))
         (next (set-name qname arr start)))

    (set-upper8 qtype arr next)
    (set-lower8 qtype arr  (+ next 1))
    
    (set-upper8 qclass arr (+ next 2))
    (set-lower8 qclass arr (+ next 3))

    (+ next 4)))


(defmethod unparse ((header header) arr start)
  "arrのstart以降にheaderを入れる"

  (let* ((h header)

         (id (header.id h))
         (qr (header.qr h))
         (opcode (header.opcode h))
         (aa (header.aa h))
         (tc (header.tc h))
         (rd (header.rd h))
         
         (ra (header.ra h))
         (z (header.z h))
         (ad (header.ad h))
         (cd (header.cd h))
         (rcode (header.rcode h))

         (qdcount (header.qdcount h))
         (ancount (header.ancount h))
         (nscount (header.nscount h))
         (arcount (header.arcount h)))

    (set-upper8 id arr start)
    (set-lower8 id arr (+ start 1))

    (setf (aref arr (+ start 2)) 
          (+ (* qr 128) (* opcode 8) (* aa 4) (* tc 2) rd)

          (aref arr (+ start 3)) 
          (+ (* ra 128) (* z 64) (* ad 32) (* cd 16) rcode))

    (set-upper8 qdcount arr (+ 4 start))
    (set-lower8 qdcount arr (+ 5 start))

    (set-upper8 ancount arr (+ 6 start))
    (set-lower8 ancount arr (+ 7 start))

    (set-upper8 nscount arr (+ 8 start))
    (set-lower8 nscount arr (+ 9 start))

    (set-upper8 arcount arr (+ 10 start))
    (set-lower8 arcount arr (+ 11 start)))
  12)



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
   ;; sizeは、sizeof-directで計算された配列のサイズ"

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
