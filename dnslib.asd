#|
  This file is a part of dnslib project.
|#

(in-package :cl-user)
(defpackage dnslib-asd
  (:use :cl :asdf))
(in-package :dnslib-asd)

(defsystem dnslib
  :version "0.1"
  :author ""
  :license ""
  :depends-on (:usocket)
  :components ((:module "src"
                :components
                ((:file "dnslib")
                 (:module "core"
                  :serial t
                  :components
                   ((:file "errors")
                    (:file "types")
                    (:file "rdataparser")
                    (:file "parser")
                    (:file "unparser")))
                 (:module "resolver"
                  :serial t
                  :components
                   ((:file "logic"))))))

  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op dnslib-test))))
