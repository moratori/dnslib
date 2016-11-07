(in-package :cl-user)
(defpackage dnslib.core.types
  (:use :cl)
  (:export 
    :dns
    :dns.header
    :dns.question
    :dns.answer
    :dns.authority
    :dns.additional

    :header
    :header.id
    :header.qr
    :header.opcode
    :header.aa
    :header.tc
    :header.rd
    :header.ra
    :header.z
    :header.ad
    :header.cd
    :header.rcode
    :header.qdcount
    :header.ancount
    :header.nscount
    :header.arcount

    :question
    :question.qname
    :question.qtype
    :question.qclass

    :rr
    :rr.name
    :rr.type
    :rr.class
    :rr.ttl
    :rr.rdlength
    :rr.rdata
    )
  )
(in-package :dnslib.core.types)



(defstruct (dns (:conc-name dns.)
                (:constructor dns))
  (header nil :type (or null header))
  (question nil :type list)
  (answer   nil :type list)
  (authority nil :type list)
  (additional nil :type list))

(defstruct (header (:conc-name header.)
                   (:constructor header)) 
  (id 0 :type (integer 0 65535))

  (qr 0 :type bit)
  (opcode 0 :type (integer 0 15))
  (aa 0 :type bit)
  (tc 0 :type bit)
  (rd 0 :type bit)

  (ra 0 :type bit)
  (z  0 :type bit)
  (ad 0 :type bit)
  (cd 0 :type bit)
  (rcode 0 :type (integer 0 15))

  (qdcount 0 :type (integer 0 65535))
  (ancount 0 :type (integer 0 65535))
  (nscount 0 :type (integer 0 65535))
  (arcount 0 :type (integer 0 65535)))

(defstruct (question (:conc-name question.)
                     (:constructor question)) 
  (qname nil :type list)
  (qtype 0  :type  (integer 0 65535))
  (qclass 0 :type (integer 0 65535)))

(defstruct (rr (:conc-name rr.)
               (:constructor rr))
  (name nil :type list)
  (type 0   :type (integer 0 65535))
  (class 0  :type (integer 0 65535))
  (ttl 0    :type (integer 0 4294967295))
  (rdlength 0 :type (integer 0 65535))

  ;; rdata のパースは、 rr構造体のrdataのみを
  ;; 参照してパースできる場合と、
  ;; 生の配列を参照しなければパースできない場合がある
  ;; (rdataにCNAMEが入ってる場合は、ドメイン名の圧縮が行われている可能性があるため)
  ;; そのため、 rdataが生の配列のどこから始まるかを arrayptrで保持しておく
  (arrayptr 0 :type (integer 0 65535))

  (rdata nil :type (or null array)))

