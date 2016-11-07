(in-package :cl-user)
(defpackage dnslib.core.rtypes
  (:use :cl) 
  (:export 
    :a
    :a.octets

    :aaaa
    :aaaa.octets
    )
  )
(in-package :dnslib.core.rtypes)


(defstruct (a (:conc-name a.)
              (:constructor a ))
  (octets nil)
  )

(defstruct (aaaa (:conc-name aaaa.)
                 (:constructor aaaa ))
  (octets nil)
  )

