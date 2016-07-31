(in-package :cl-user)
(defpackage dnslib.core.parser
  (:use :cl
        :dnslib.core.types
        :dnslib.core.errors
        )
  (:export 
    :parse
    )
  )
(in-package :dnslib.core.parser)



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



(defun parse-name (ubyte-array len start initial-start &optional (flag t))
  "以下を多値で返す:
     - 次のパースで処理を開始すべきポインタ
     - DNSの各ヘッダ中で出現するドメイン名をパースし、
       ラベルのリストにしたもの."
 
  (declare (type (simple-array (unsigned-byte 8)) ubyte-array))
  (declare (type fixnum len start initial-start))
  
  (when (and (<= initial-start start) (not flag))
    (qp-error ubyte-array "pointer must be point foregoing data"))
  
  (let ((result nil))

    (unless (< start len)
      (qp-error ubyte-array 
                (format nil "pointer(~A) must be less than ~A"
                        start len)))
    
    (loop 
      named exit
      for label-len = (aref ubyte-array start)
      for msb2 = (ash label-len -6) ;; 上位2bitを計算
      do 
      (cond 

        ;; 圧縮されていない場合の処理
        ((zerop msb2) 

         (when (zerop label-len)
           (incf start)
           (return-from exit))

         (unless (<= (1+ start) (+ 1 start label-len) len)
           (ql-error ubyte-array "label length too long for array"))

         (setf result (nconc result (list (subseq ubyte-array (1+ start) (+ 1 start label-len)))))
         (incf start (1+ label-len))

         (when (< (1- len) start)
           (qt-error ubyte-array "qname must be end with zero" )))

        ;; 上位2bitが11だったら圧縮されてる
        ((= msb2 3) 

           (unless (< (1+ start)  len)
             (qp-error ubyte-array "14bit is required for compression pointer"))

           (let* ((next (aref ubyte-array (1+ start)))
                  (jump-ptr (logior (ash (logand label-len #X3F) 8) next)))

             (when (<= 0 jump-ptr 11)
               (qp-error ubyte-array "pointer must not point header"))

             (multiple-value-bind 
               (_ tmp) (parse-name ubyte-array len jump-ptr initial-start nil)
               (declare (ignore _))
               (setf result (nconc result tmp))
               (incf start 2)
               (return-from exit))))
        (t 
         (qp-error ubyte-array "malformed upper bit"))))
    
    (values start result)))



(defun parse-header (placef dns ubyte-array len start cnt)
  "配列ubyte-arrayの0-12を見てパースしplacefを呼び出し
   dnsの適当な場所にセットする
   また、次のパースで処理を開始すべきポインタを返す"

  (declare (ignore len start cnt))
  (declare (type (simple-array (unsigned-byte 8)) ubyte-array))

  (destructuring-bind 
      (id tmp qdcount ancount nscount arcount)
      (loop for i from 0 upto 10 by 2
            collect (concat-byte 
                      (aref ubyte-array i)
                      (aref ubyte-array (1+ i))))
      
      (funcall 
        (fdefinition `(setf ,placef))
        (header 
          :id id

          :qr     (ash (logand tmp #X8000 ) -15)
          :opcode (ash (logand tmp #X7800 ) -11)
          :aa     (ash (logand tmp #X0400 ) -10)
          :tc     (ash (logand tmp #X0200 ) -9)
          :rd     (ash (logand tmp #X0100 ) -8)
          :ra     (ash (logand tmp #X0080 ) -7)
          :z      (ash (logand tmp #X0040 ) -6)
          :ad     (ash (logand tmp #X0020 ) -5)
          :cd     (ash (logand tmp #X0010 ) -4)
          :rcode  (logand tmp #X000F ) 

          :qdcount qdcount
          :ancount ancount
          :nscount nscount
          :arcount arcount)

        dns)
      12))


(defun %parse-qn-ty-cl (ubyte-array len start)
  "aaaの各セクションのはじめの部分と、questionセクション
   は共通しているため、この関数で qname type class をパースし
   多値で返す."

  (declare (type (simple-array (unsigned-byte 8)) ubyte-array))
  (declare (type fixnum len start))

  (multiple-value-bind  
    (ptr qname) 
    (parse-name ubyte-array len start start)
              
    (unless (< (+ ptr 3) len)
      (mq-error ubyte-array "(name-type-class) parts too short"))

    (values 
      (+ ptr 4)
      qname
      (concat-byte (aref ubyte-array ptr) (aref ubyte-array (1+ ptr)))
      (concat-byte (aref ubyte-array (+ ptr 2)) (aref ubyte-array (+ ptr 3))))))


(defun parse-question (placef dns ubyte-array len start cnt)
  "配列ubyte-arrayのstartからcnt個存在するであろう
   questionをパースをしplacefを呼び出しdnsの適当な場所にセットする
   また、次のパースで処理を開始すべきポインタを返す"

  (declare (type (simple-array (unsigned-byte 8)) ubyte-array))
  (declare (type fixnum len start cnt))
 
  (let ((next start))

    (funcall 
      (fdefinition `(setf ,placef))
      (loop repeat cnt
            collect
            (multiple-value-bind  
              (ptr qname qtype qclass) 
              (%parse-qn-ty-cl ubyte-array len next)

              (setf next ptr)

              (question 
                :qname qname
                :qtype qtype
                :qclass qclass)))
      dns)
    next))



(defun parse-aaa (placef dns ubyte-array len start cnt)
  "配列ubyte-arrayのstartからcnt個存在するであろう
   answer,authority,addtionalのいずれか(全て同じフォーマット)をパースをし
   placefを呼び出しdnsの適当な場所にセットする
   また、次のパースで処理を開始すべきポインタを返す"
  
  (declare (type (simple-array (unsigned-byte 8)) ubyte-array))
  (declare (type fixnum len start cnt))
  
  (let ((next start))

    (funcall 
      (fdefinition `(setf ,placef)) 
      (loop 
        repeat cnt
        collect 
        (multiple-value-bind 
          (ptr name type class) 
          (%parse-qn-ty-cl ubyte-array len next)

          (unless (< (+ ptr 5) len)
            (aaa-error ubyte-array "aaa section too short: can't read TTL"))

          (let ((ttl 
                  (concat-short 
                    (concat-byte (aref ubyte-array ptr) (aref ubyte-array (1+ ptr)))
                    (concat-byte (aref ubyte-array (+ ptr 2)) (aref ubyte-array (+ ptr 3)))))
                (rdlength 
                  (concat-byte 
                    (aref ubyte-array (+ ptr 4)) (aref ubyte-array (+ ptr 5)))))
            
            (unless (< (+ ptr 5 rdlength) len)
              (aaa-error ubyte-array "aaa section too short: can't read RDATA"))

            (setf next (+ ptr 5 rdlength 1)) 

            (rr
              :name name
              :type type
              :class class
              :ttl ttl
              :rdlength rdlength
              :rdata (subseq ubyte-array (+ ptr 6) (+ ptr 6 rdlength))))))
      dns)
    next))



(defun parse (ubyte-array)
  "unsigned-byteの配列からdns構造体を作って返す
   header,question,answer,authority,additionalの各セクションを実際にパースする
   処理を呼び出し、dns構造体に破壊的にセットする"

  (declare (type (simple-array (unsigned-byte 8)) ubyte-array))
  
  (let ((len (length ubyte-array))
        (dns (dns))
        (pointer 0))

    (when (< len 12)
      (dp-error ubyte-array "malformed dns header(too short)")) 
    
    (setf pointer (parse-header   'dns.header     dns ubyte-array len pointer 1)
          pointer (parse-question 'dns.question   dns ubyte-array len pointer (header.qdcount (dns.header dns)))
          pointer (parse-aaa      'dns.answer     dns ubyte-array len pointer (header.ancount (dns.header dns)))
          pointer (parse-aaa      'dns.authority  dns ubyte-array len pointer (header.nscount (dns.header dns)))
          pointer (parse-aaa      'dns.additional dns ubyte-array len pointer (header.arcount (dns.header dns))))

    dns))

