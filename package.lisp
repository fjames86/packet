
;;
;; Package for packing/unpacking strutures into/out of Lisp buffers. 
;; Can be used for e.g. UDP packets or mmap'ed data structures.
;; 
;; Copyright Frank James 
;; March 2014
;;

(defpackage :packet
  (:use :cl)
  (:export #:defpacket
	   #:defpacket*
		   #:pack
		   #:unpack
		   #:type-size
		   #:list-types
           #:hd
           #:subseq*
           #:pad
	   #:pad*
           #:usb8
           #:usb8*
           #:defflags
           #:pack-flags
           #:unpack-flags
           #:flag-p
           #:defenum
           #:enum
           #:enum-p
           #:enum-id))
