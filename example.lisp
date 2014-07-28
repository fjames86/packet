

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

;;;;;;;;;;;;;;;;;;;;;;;;;
;; usocket UDP example
;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *server-thread* nil)
(defparameter *server-exiting* nil)
(defparameter *remote-host* nil)
(defparameter *remote-port* nil)

;; client
(defun send-msg (host port msg)  
  "Send a message to a UDP server"
  (let ((socket (usocket:socket-connect host port
										:protocol :datagram
										:element-type '(unsigned-byte 8))))
	(unwind-protect
		 (usocket:socket-send socket msg (length msg))
	  (usocket:socket-close socket))))

;; server framework
(defun start-udp-server (handler port &key (timeout 1) (max-buffer-size 4096))
  "Start example UDP server"
  (labels ((main-loop ()			 
			 (setf *server-exiting* nil
				   *server-thread* (bt:current-thread))
			 (let ((socket (usocket:socket-connect nil 0
												   :protocol :datagram
												   :element-type '(unsigned-byte 8)
												   :timeout timeout
												   :local-port port)))
			   (unwind-protect
					(do ((buffer (make-array max-buffer-size
											 :element-type '(unsigned-byte 8))))
						(*server-exiting*)
					  (multiple-value-bind (recv n *remote-host* *remote-port*)
						  (usocket:socket-receive socket buffer max-buffer-size)
						(declare (ignore recv))
						(funcall handler (subseq buffer 0 n))))
				 (usocket:socket-close socket)))))
	(bt:make-thread #'main-loop :name (format nil "UDP-SERVER ~D" port))))

(defun stop-udp-server (port)
  "Stop example UDP server"
  ;; tell the server to exit
  (setf *server-exiting* t)
  ;; send a dummy message to clean things up
  (send-msg "localhost" port (usb8* 0))
  ;; wait for the thread to exit
  (bt:join-thread *server-thread*)
  ;; done
  nil)


;; example server that prints the buffer sent 
(defconstant +example-port+ 8000)
(defparameter *example-stream* *error-output*)

(defun example-handler (buffer)
  (format *example-stream* "HOST: ~A PORT: ~A~%"
		  (usocket:vector-quad-to-dotted-quad *remote-host*)
		  *remote-port*)
  (let ((*standard-output* *example-stream*))
	(hd buffer))  
  nil)

(defun start-example-server ()
  (start-udp-server 'example-handler +example-port+))

(defun stop-example-server ()
  (stop-udp-server +example-port+))
 