(in-package :cl-user)
(defpackage dnslib.core.unparser
  (:use :cl
        :dnslib.core.types
        :dnslib.core.errors
        )
  (:export 
    :unparse
    )
  )
(in-package :dnslib.core.unparser)







(defun unparse (dns)
  "DNS構造体からunsigned-byte 8 な配列を返す"

  (declare (type dns dns))


  )
