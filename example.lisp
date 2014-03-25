

(defpackage :packet-example 
  (:use :cl :packet))

(in-package :packet-example)


#|

;; example C structure that we want to model:

struct example_s {
  uint32_t num;
  uint32_t morenums[10];
  char name[20];
} __attribute__ ((packed));

|#

(defpacket example-s 
  ((num :uint32 :initform 0 :initarg :num :accessor example-s-num)
   (morenums (:uint32 10) :initform nil :initarg :morenums :accessor example-s-morenums)
   (name (:string 20) :initform "" :initarg :name :accessor example-s-name))
  (:packing 1)
  (:documentation "Example packet structure."))

(defun pack-example (num morenums name)
  "Get a packed buffer from an example-s object."
  (pack (make-instance 'example-s 
		       :num num
		       :morenums morenums
		       :name name)
	'example-s))

(defun unpack-example (buffer)
  "Get an example-s object from the buffer."
  (unpack buffer 'example-s))


(defun example-test (num morenums name)
  "Make a packet and unpack it. Compare the unpacked slots with the original values."
  (let ((start (make-instance 'example-s
			      :num num
			      :morenums morenums
			      :name name)))
    (let ((end (unpack (pack start 'example-s)
		       'example-s)))
      (assert (= (example-s-num end) num))
      (assert (every #'= (example-s-morenums end) morenums))
      (assert (string= (example-s-name end) name))))
  t)



	
    
