(in-package :cl-user)
(defpackage dnslib-test
  (:use :cl
        :prove
        ))
(in-package :dnslib-test)

;; NOTE: To run this test file, execute `(asdf:test-system :dnslib)' in your Lisp.


(setf prove:*debug-on-error* t)
(setf prove:*default-reporter* :fiveam)
(setf *random-state* (make-random-state t))



(defvar *random-dns-packet-test* 1000000)


(defun random-array-maker (len element-range)
  (make-array (list len)
              :element-type 'unsigned-byte
              :initial-contents 
              (loop for i from 0 below len collect
                    (random element-range))))

(defvar *failers* nil)

(defun test-result-mapper ()
  (let ((d (random-array-maker (random 4096) 256) ))
    (handler-case
      (dnslib.core.parser:parse d)
      (dnslib.core.errors::data-parse-error (err) 
        (declare (ignore err))
        t)
      (condition (err) 
        (print err) 
        (push d *failers*)
        nil))))


(plan 1)

(subtest "TESTING: dnslib.core.parser"

        (let* ((tcase_parse-name1 
                 (make-array '(16) 
                             :element-type 'unsigned-byte  
                             :initial-contents '(3 119 119 119 7 101 120 97 109 112 108 101 2 106 112 0)))

               (tcase_parse-header1_1
                 (dnslib.core.types:dns))
               (tcase_parse-header1_2
                 (make-array '(12) 
                             :element-type 'unsigned-byte  
                             :initial-contents '(#X7c #Xd9 #X01 #X00 #X00 #X01 #X00 #X00 #X00 #X00 #X00 #X00)))

               (tcase_parse-question1_1
                 (dnslib.core.types:dns))
               (tcase_parse-question1_2
                 (make-array '(20)
                             :element-type 'unsigned-byte
                             :initial-contents 
                            '(#X03 #X77 #X77 #X77 #X06 #X66 #X6f #X6f #X62 #X61 #X72 #X03 #X63 #X6f #X6d #X00 #X00 #X01 #X00 #X01)))
               (tcase_parse-question2_1
                 (dnslib.core.types:dns))
               (tcase_parse-question2_2
                 (make-array '(20)
                             :element-type 'unsigned-byte
                             :initial-contents 
                            '(#X03 #X77 #X77 #X77 #X06 #X67 #X6f #X6f #X67 #X6c #X65 #X03 #X63 #X6f #X6d #X00 #X00 #X01 #X00 #X01)))
               (tcase_parse-question3_1
                 (dnslib.core.types:dns))
               (tcase_parse-question3_2
                 (make-array '(16)
                             :element-type 'unsigned-byte
                             :initial-contents 
                             '(#X07 #X65 #X78 #X61 #X6d #X70 #X6c #X65 #X02 #X6a #X70 #X00 #X00 #X01 #X00 #X01)))
               (tcase_parse-question4_1
                 (dnslib.core.types:dns))
               (tcase_parse-question4_2
                 (make-array '(13)
                             :element-type 'unsigned-byte
                             :initial-contents 
                             '(#X04 #X6d #X74 #X63 #X71 #X02 #X6a #X70 #X00 #X00 #X01 #X00 #X01)))
               (tcase_parse-question5_1
                 (dnslib.core.types:dns))
               (tcase_parse-question5_2
                 (make-array '(15)
                             :element-type 'unsigned-byte
                             :initial-contents 
                             '(#X05 #X61 #X73 #X61 #X68 #X69 #X03 #X63 #X6f #X6d #X00 #X00 #X02 #X00 #X01 )))
               (tcase_parse-question6_1
                 (dnslib.core.types:dns))
               (tcase_parse-question6_2
                 (make-array '(17)
                             :element-type 'unsigned-byte
                             :initial-contents 
                             '(#X05 #X79 #X61 #X68 #X6f #X6f #X02 #X63 #X6f #X02 #X6a #X70 #X00 #X00 #X0f #X00 #X01)))

               (raws 
                 
                 '(

                   #|
(
181 167 32 121 192 229 183 4 163 84 119 226 192 12 47 44 40 243 119 155 48 67
  47 95 75 96 55 147 114 31 114 48 104 205 232 253 90 36 43 56 42 129 89 244 87
  216 32 80 122 59 0 213 182 21 215 95 142 116 213 74 241 75 68 101 147 192 85
  96 2 46 136 84 61 116 233 206 124 44 216 223 223 88 89 176 91 1 21 54 15 123
  218 179 172 112 113 72 169 225 217 217 27 159 180 123 232 166 33 235 225 147
  27 247 99 52 102 120 97 179 21 13 36 128 84 133 80 106 207 242 73 172 78 28
  61 92 89 113 236 126 168 221 218 167 69 199 11 196 6 110 234 211 216 176 4 26
  34 80 211 115 157 52 136 182 63 116 99 248 68 76 137 128 161 55 111 168 233
  232 231 41 209 218 142 47 252 208 248 101 5 185 172 184 160 145 17 87 1 143
  170 86 216 121 137 90 11 197 195 169 25 169 156 121 73 209 118 148 47 121 135
  55 69 52 238 187 166 241 138 221 132 251 148 96 187 58 8 111 196 156 218 186 250 123 218 0 246 170 122 172 152
)
|#
                   (#X59 #X1b #X01 #X00 #X00 #X01 #X00 #X00 #X00 #X00 #X00 #X00 #X03 #X77 #X77 #X77
                    #X06 #X67 #X6f #X6f #X67 #X6c #X65 #X02 #X63 #X6f #X02 #X6a #X70 #X00 #X00 #X01 #X00 #X01)
                   (#Xcd #X7f #X81 #X80 #X00 #X01 #X00 #X02 #X00 #X00 #X00 #X00 #X08 #X63 #X6c #X69
                    #X65 #X6e #X74 #X73 #X31 #X06 #X67 #X6f #X6f #X67 #X6c #X65 #X03 #X63 #X6f #X6d
                    #X00 #X00 #X01 #X00 #X01 #Xc0 #X0c #X00 #X05 #X00 #X01 #X00 #X00 #X00 #Xa2 #X00
                    #X0c #X07 #X63 #X6c #X69 #X65 #X6e #X74 #X73 #X01 #X6c #Xc0 #X15 #Xc0 #X31 #X00
                    #X01 #X00 #X01 #X00 #X00 #X00 #Xa2 #X00 #X04 #Xd8 #X3a #Xc8 #Xae)
                   (#X07 #Xed #X81 #X80 #X00 #X01 #X00 #X02 #X00 #X00 #X00 #X00 #X04 #X6f #X63 #X73
                    #X70 #X08 #X64 #X69 #X67 #X69 #X63 #X65 #X72 #X74 #X03 #X63 #X6f #X6d #X00 #X00
                    #X01 #X00 #X01 #Xc0 #X0c #X00 #X05 #X00 #X01 #X00 #X01 #X38 #X73 #X00 #X14 #X03
                    #X63 #X73 #X39 #X03 #X77 #X61 #X63 #X06 #X70 #X68 #X69 #X63 #X64 #X6e #X03 #X6e
                    #X65 #X74 #X00 #Xc0 #X2f #X00 #X01 #X00 #X01 #X00 #X00 #X06 #X73 #X00 #X04 #X75
                    #X12 #Xed #X1d )
                   (
                     #X8f #X71 #X81 #X80 #X00 #X01 #X00 #X02 #X00 #X00 #X00 #X01 #X04 #X6a #X70 #X72
                     #X73 #X02 #X6a #X70 #X00 #X00 #X01 #X00 #X01 #Xc0 #X0c #X00 #X01 #X00 #X01 #X00
                     #X00 #X01 #X2c #X00 #X04 #X75 #X68 #X85 #Xa4 #Xc0 #X0c #X00 #X2e #X00 #X01 #X00
                     #X00 #X01 #X2c #X00 #X9b #X00 #X01 #X08 #X02 #X00 #X00 #X01 #X2c #X57 #Xaf #Xd7
                     #Xac #X57 #X88 #X4a #Xac #X19 #X9b #X04 #X6a #X70 #X72 #X73 #X02 #X6a #X70 #X00
                     #X39 #Xb5 #Xf2 #X47 #X95 #X89 #Xf6 #X3c #X1f #X2c #Xc4 #Xf9 #Xde #Xfd #Xc9 #X5d
                     #X3e #Xc7 #Xb5 #Xdd #X9d #X74 #X89 #Xd5 #X83 #X5a #X09 #Xef #Xae #Xeb #X16 #X8e
                     #Xd1 #Xec #X5a #X22 #X7a #X07 #X13 #Xd8 #Xe6 #X86 #Xc8 #X1d #X56 #Xbc #X08 #Xc0
                     #Xe3 #Xb9 #X89 #X4b #X75 #Xc3 #X8e #X8d #Xb0 #Xe6 #X29 #X2d #X44 #Xef #X9b #Xa6
                     #Xdb #X0d #X54 #X3a #Xbc #X76 #X92 #Xf8 #X2f #X0a #X9d #Xca #X56 #X21 #Xab #Xda
                     #X10 #Xcb #X6e #X05 #Xbf #Xe1 #X38 #X70 #X7b #X77 #Xd5 #X46 #X1e #X5e #X14 #X91
                     #Xbd #X32 #X66 #X0f #Xfb #X4d #X21 #X3e #Xf4 #X5f #Xb2 #Xe6 #Xf9 #Xaa #Xae #X9f
                     #X0c #X14 #X6a #X9b #Xe9 #Xc1 #X6c #Xf4 #X75 #X52 #X90 #X2e #X1a #Xec #X54 #Xe7
                     #X00 #X00 #X29 #X05 #X00 #X00 #X00 #X80 #X00 #X00 #X00
                    )
                   ( 
                     #X8f #X71 #X01 #X20 #X00 #X01 #X00 #X00 #X00 #X00 #X00 #X01 #X04 #X6a #X70 #X72
                     #X73 #X02 #X6a #X70 #X00 #X00 #X01 #X00 #X01 #X00 #X00 #X29 #X10 #X00 #X00 #X00
                     #X80 #X00 #X00 #X00
                     )))
               
               
               (whole 
                 (loop for each in raws collect 
                       (make-array (list (length each))
                                   :element-type 'unsigned-byte
                                   :initial-contents each))))


        (is (dnslib.core.parser::concat-byte 0 0) 0)
        (is (dnslib.core.parser::concat-byte 255 255) 65535)
        (is (dnslib.core.parser::concat-byte 13 34) 3362)

        (is (dnslib.core.parser::concat-short 0 0) 0)
        (is (dnslib.core.parser::concat-short 65535 65535) 4294967295)
        (is (dnslib.core.parser::concat-short 255 65535) 16777215)
         
        (ok (dnslib.core.parser::parse-name tcase_parse-name1 0 0))

        (ok (dnslib.core.parser::parse-header 'dnslib.core.types:dns.header tcase_parse-header1_1 tcase_parse-header1_2 0 1)) 

        (ok (dnslib.core.parser::parse-question 'dnslib.core.types:dns.question tcase_parse-question1_1 tcase_parse-question1_2 0 1))
        (ok (dnslib.core.parser::parse-question 'dnslib.core.types:dns.question tcase_parse-question2_1 tcase_parse-question2_2 0 1))
        (ok (dnslib.core.parser::parse-question 'dnslib.core.types:dns.question tcase_parse-question3_1 tcase_parse-question3_2 0 1))
        (ok (dnslib.core.parser::parse-question 'dnslib.core.types:dns.question tcase_parse-question4_1 tcase_parse-question4_2 0 1))
        (ok (dnslib.core.parser::parse-question 'dnslib.core.types:dns.question tcase_parse-question5_1 tcase_parse-question5_2 0 1))
        (ok (dnslib.core.parser::parse-question 'dnslib.core.types:dns.question tcase_parse-question6_1 tcase_parse-question6_2 0 1))
        
        (loop for each in whole do (ok (dnslib.core.parser:parse each)))

        (loop for i from 0 below *random-dns-packet-test*
              do (ok (test-result-mapper)))
        
        ))

(print *failers*)

(finalize)

