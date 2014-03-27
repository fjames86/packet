

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

;; try sending the packet over udp using usocket
(defun send-example (num morenums name &key (host "localhost") (port 8000))
  (let ((buffer (pack-example num morenums name))
		(socket (usocket:socket-connect host port
										:protocol :datagram)))	
	(usocket:socket-send socket buffer (length buffer))))


;; -----------------

;; more complicated example showing nested structures

#|

#define MAX_NAME 20
#define MAX_PEOPLE 10
struct person_s {
  char name[MAX_NAME];
  uint32_t age;
};

struct family_s {
  char surname[MAX_NAME];
  uint32_t npeople;
  struct person_s people[MAX_PEOPLE];
};

|#

(defconstant +max-name+ 20)
(defconstant +max-people+ 10)

(defpacket person-s
  ((name (:string +max-name+) :initform "" :initarg :name)
   (age :uint32 :initform 0 :initarg :age))
  (:documentation "Person structure."))

(defpacket family-s
  ((surname (:string +max-name+) :initform "" :initarg :surname)
   (npeople :uint32 :initform 0 :initarg :npeople)
   (people (person-s +max-people+) :initform nil :initarg :people))
  (:documentation "Family structure."))

