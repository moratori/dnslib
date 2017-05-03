(in-package :cl-user)
(defpackage dnslib.core.evalrdata
  (:use :cl
        :dnslib.core.types
        :dnslib.core.errors
        :dnslib.core.util
        )
  (:export 
    )
  (:documentation
    "DNS構造体のRR中のrdata部分のバイト配列をパースし、
     適当な構造体にする"))
(in-package :dnslib.core.evalrdata)


