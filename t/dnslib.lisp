(in-package :cl-user)
(defpackage dnslib-test
  (:use :cl
        :prove
        :dnslib.core.errors
        :dnslib.core.types
        ))
(in-package :dnslib-test)

;; NOTE: To run this test file, execute `(asdf:test-system :dnslib)' in your Lisp.


(setf prove:*debug-on-error* t)
(setf prove:*default-reporter* :fiveam)
(setf *random-state* (make-random-state t))

(defvar *real-dnspayload* (eval (read (open "t/dnspayload"))))
(defvar *random-dns-packet-test* 1)

(defun make-ub8-array (list)
  (make-array 
    (list (length list))
    :element-type '(unsigned-byte 8)
    :initial-contents list))

(defun make-random-array (len element-range)
  (let ((random-list 
          (loop 
            repeat len
            collect (random element-range))))
    (make-ub8-array random-list)))



(plan 3)

(time 
  (progn 

    (format t "~%~%~%")

    (subtest "########## TESTING: PARSE(RANDOM_PAYLOAD) ##########"
             (loop repeat *random-dns-packet-test* 
                   do 
                   (ok
                     (let ((in (make-random-array (random 8192) 256)))
                       (handler-case 
                         (dnslib.core.parser:parse in)
                         (data-parse-error (err)
                           (declare (ignore err))
                           t))))))

    (subtest "########## TESTING: PARSE REAL PAYLOAD ##########"
             (loop for _ in *real-dnspayload*
                   for raw  = (make-ub8-array _) 
                   do
                   (ok 
                     (let* ((parsed-1 (dnslib.core.parser:parse raw)))
                       parsed-1))))

    (subtest "########## TESTING: PARSE(UNPARSE(PARSE(REAL_PAYLOAD))) ##########"
             (loop for _ in *real-dnspayload*
                   for raw  = (make-ub8-array _)
                   do
                   (ok 
                     (handler-case 
                       (let*  ((parsed-1 (dnslib.core.parser:parse raw))
                               (parsed-2 (dnslib.core.parser:parse 
                                           (dnslib.core.unparser:unparse 
                                             parsed-1
                                             (dnslib.core.unparser:sizeof parsed-1))))
                               (parsed-3 (dnslib.core.parser:parse 
                                           (dnslib.core.unparser:unparse 
                                             parsed-2
                                             (dnslib.core.unparser:sizeof parsed-2)))))

                         (if (equalp parsed-2 parsed-3)
                             t
                             (progn (print parsed-2)
                                    (print parsed-3) 
                                    nil)))))))
    ))

(finalize)


(sb-cover:report 
  (merge-pathnames #P"coverage/"
    (asdf:system-source-directory :dnslib)))

