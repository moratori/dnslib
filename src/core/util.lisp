(in-package :cl-user)
(defpackage dnslib.core.util
  (:use :cl
        :dnslib.core.types
        :dnslib.core.errors
        )
  (:export 
    :concat-byte
    :concat-short
    :summation
    :set-upper8
    :set-lower8
    :name-len
    :set-name
    :%parse-name
    :to-ubyte8-array
    )
  )
(in-package :dnslib.core.util)


(defun summation (list fn)
  (reduce 
    (lambda (r x)
      (+ r (funcall fn x)))
    list 
    :initial-value 0))


(defun to-domain-name (list)
  (loop 
    for label in list
    for llabel = (coerce label 'list)
    collect (concatenate 'string 
                         (mapcar #'code-char llabel))))


(defun to-ubyte8-array (domain-name)
  (loop 
    for label-str in domain-name
    collect
    (let ((res-label 
            (make-array (list (length label-str))
                        :element-type '(unsigned-byte 8)
                        :initial-element 0)))
      (loop for ch across label-str
            for i from 0
            do (setf (aref res-label i) (char-code ch)))
      res-label)))


(defun %parse-name (ubyte-array len start &key (initial-start start) (firststep t) (evalstep nil))

  (multiple-value-bind 
    (cf ptr name) (%%parse-name ubyte-array len start 
                                :initial-start initial-start
                                :firststep firststep
                                :evalstep  evalstep)
    (values cf ptr (to-domain-name name))))


(defun %%parse-name (ubyte-array len start &key (initial-start start) (firststep t) (evalstep nil))
  "ubyte-arrayのstartから始めて、ドメイン名のリストを作成する
   ドメイン名が圧縮されている場合は、ポインタをたどり ドメイン名を取得しにいく

   戻り値: (次のパースの始まりのポインタ, ラベルのリスト)
   %PARSE-NAME :: simple-array(unsigned-byte(8)) -> FIXNUM -> FIXNUM -> FIXNUM -> BOOLEAN -> (FIXNU, LIST)"
 
  (declare (type (simple-array (unsigned-byte 8)) ubyte-array))
  (declare (type fixnum len start initial-start))
  (declare (type boolean firststep evalstep))

  (when (and (<= initial-start start) (not firststep))
    (qp-error ubyte-array "pointer must be point foregoing data"))
  (unless (< start len)
    (qp-error ubyte-array 
              (format nil "pointer(~A) must be less than ~A" start len)))
  (when (<= 0 start 11)
    (qp-error ubyte-array "pointer must not point header"))
  
  (let ((result nil)
        (continue-flag nil)) ;; evalstepが真であり、ジャンプが発生する場合、"大元"の ubyte-array を参照する
                             ;; 必要があるためcontinue-flagを真にする
     
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
              (when evalstep
                (setf continue-flag t
                      start         jump-ptr)
                (return-from exit))
              (multiple-value-bind 
                (cf _ tmp) (%%parse-name ubyte-array len jump-ptr 
                                     :initial-start initial-start
                                     :firststep nil)
                (declare (ignore _))
                (declare (ignore cf))
                (setf result (nconc result tmp))
                (incf start 2)
                (return-from exit))))
         (t 
          (qp-error msb2 "malformed upper bit"))))
     (values continue-flag start result)))



(defun name-len (name)
  "nameは配列のリスト
   unsigned-byte 8な配列に変換するに当たって何バイト使うか
   計算して返す
   
   戻り値: 配列の長さ
   NAME-LEN :: LIST -> FIXNUM"
  
  (+ 1 
     (length name)
     (loop for each in name sum (length each))))


(defun set-name (name arr start)
  "nameをarrのstartから入れて次のポインタを返す
   
   戻り値: 次に処理を開始すべきポインタ
   SET-NAME :: LIST -> simple-array(unsigned-byte(8)) -> FIXNUM -> FIXNUM"

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
  "上位1byteをarrのstartにセットする
   
   戻り値: 次に処理を開始すべきポインタ
   SET-UPPER8 :: FIXNUM -> simple-array(unsigned-byte(8)) -> FIXNUM -> FIXNUM"

  (setf (aref arr start) (ash n -8))
  (1+ start))

(defun set-lower8 (n arr start)
  "下位1byteをarrのstartにセットする
   
   戻り値: 次に処理を開始すべきポインタ
   SET-LOWER8 :: FIXNUM -> simple-array(unsigned-byte(8)) -> FIXNUM -> FIXNUM"

  (setf (aref arr start) (logand n 255))
  (1+ start))



(defun concat-byte (msb lsb)
  "msbを上位８bit、lsbを下位8bitとする16bitの数を返す
   
   戻り値: 16bitに収まる長さの数
   CONCAT-BYTE :: FIXNUM -> FIXNUM -> FIXNUM"
  
  (declare (type fixnum msb lsb))
  (logior (ash msb 8) lsb))


(defun concat-short (msb lsb)
  "msbを上位16bit、lsbを下位16bitとする32bitの数を返す

   戻り値: 32bitに収まる長さの数
   CONCAT-SHORT :: FIXNUM -> FIXNUM -> FIXNUM"

  (declare (type fixnum msb lsb))
  (logior (ash msb 16) lsb))
 





