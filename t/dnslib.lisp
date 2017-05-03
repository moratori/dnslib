(in-package :cl-user)
(defpackage dnslib-test
  (:use :cl
        :prove
        :dnslib.core.errors
        ))
(in-package :dnslib-test)

;; NOTE: To run this test file, execute `(asdf:test-system :dnslib)' in your Lisp.


(setf prove:*debug-on-error* t)
(setf prove:*default-reporter* :fiveam)
(setf *random-state* (make-random-state t))

(defvar *real-dnspayload* (eval (read (open "t/dnspayload"))))
(defvar *random-dns-packet-test* 50000)
(defvar *failers* nil)


(defun make-ub8-array (list)
  (make-array 
    (list (length list))
    :element-type '(unsigned-byte 8)
    :initial-contents list))

(defun make-random-array (len element-range)
  (make-ub8-array 
    (loop for i from 0 below len 
          collect (random element-range))))

(defun random-test ()
  (let ((d (make-random-array (random 4096) 256)))
    (handler-case
      (dnslib.core.parser:parse d)
      (dnslib.core.errors::data-parse-error (err) 
        (declare (ignore err))
        t)
      (condition (err) 
        (print err) 
        (push d *failers*)
        nil))))




(plan 2)


(subtest "TESTING: PARSE(RANDOM_PAYLOAD)"
   (loop repeat *random-dns-packet-test*
         do (ok (random-test))))


(subtest "TESTING: PARSE(UNPARSE(PARSE(REAL_PAYLOAD)))"
   (loop for _ in *real-dnspayload*
         for raw  = (make-ub8-array _)
         do
         (handler-case 
           (ok 
             (handler-case
               (let ((obj1 (dnslib.core.parser:parse raw)))
                (print obj1)
                (equalp 
                  obj1
                 (dnslib.core.parser:parse 
                  (dnslib.core.unparser:unparse-direct
                    obj1
                    (dnslib.core.unparser:sizeof-direct obj1))))) 
               (data-parse-error (err)
                 (print (mes err))
                 (print (data err))
                 nil))))))



(format *standard-output* "~%~%------------~%FATAL ERRORS~%~A~%------------~%~%" *failers*)

(finalize)


(sb-cover:report 
  (merge-pathnames #P"coverage/"
    (asdf:system-source-directory :dnslib)))
