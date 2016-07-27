#|
  This file is a part of dnslib project.
|#

(in-package :cl-user)
(defpackage dnslib-test-asd
  (:use :cl :asdf))
(in-package :dnslib-test-asd)

(defsystem dnslib-test
  :author ""
  :license ""
  :depends-on (:dnslib
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "dnslib"))))
  :description "Test system for dnslib"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
