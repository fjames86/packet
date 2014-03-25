
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
	   #:pack
	   #:unpack))

(in-package :packet)

;; idea is to operate on a statically defined, fixed size buffer
;; this means we have to store slot offsets and structure definitiosn ourselves

;; types:
;; 8, 16, 32 and 64-bit integers 
;; floats/doubles
;; arrays of primitives
;; structures
;; arrays of structures

;;
;; strings (fixed-width buffers)
;;
;; note that we don't have to concern ourselves with pointers
;; 
;;

;; alist of type definitions
(defparameter *type-definitions* (make-hash-table)
  "Global table storing type definitions.")

(defun %define-type (name pack-handler unpack-handler size)
  "Intern a new type definition. 

PACK-HANDLER is a function with lambda-list (object buffer start)
and should encode the object into the buffer starting at offset START.

UNPACK-HANDLE is a function with lambda-list (buffer start) 
and should extract information about the desired object starting at offset START. It 
should return the object.

SIZE is the total number of bytes this object consumes."
  (setf (gethash name *type-definitions*)
	(list name pack-handler unpack-handler size))
  nil)

(defun get-type (name)
  "Find the type definition."
  (gethash name *type-definitions*))

(defun type-packer (type)
  "Get the packing handler for this type."
  (when (symbolp type)
    (setf type (get-type type)))
  (second type))

(defun type-unpacker (type)
  "Get the unpacking handler for this type."
  (when (symbolp type)
    (setf type (get-type type)))
  (third type))

(defun type-size (type)
  "Get the total size for this type."
  (when (symbolp type)
    (setf type (get-type type)))
  (fourth type))

(defun %define-alias (alias name)
  "Define an alias for a type. Allows refering to the same type by a different name."
  (let ((type (get-type name)))
    (%define-type alias (type-packer type) (type-unpacker type) (type-size type))))

(defun pack-uint8 (uint buffer start)
  "Pack an unsigned byted into the buffer."
  (setf (elt buffer start) uint))

(defun unpack-uint8 (buffer start)
  "Extract an unsigned byte from the buffer."
  (elt buffer start))

(defun bytes (integer &optional (size 4))
  "Expand an integer into its consituent bytes"
  (do ((bytes nil)
       (i 0 (1+ i)))
      ((= i size) (nreverse bytes))
    (push (logand integer 255) bytes)
    (setf integer (ash integer -8))))

(defun pack-bytes (bytes buffer start)
  "Pack a list of bytes into a buffer"
  (do ((bytes bytes (cdr bytes))
       (i start (1+ i)))
      ((null bytes))
    (setf (elt buffer i) (car bytes)))
  buffer)

(defun merge-bytes (bytes &optional signed)
  "Convert a list of bytes into an unsigned integer"
  (do ((i 0 (1+ i))
       (integer 0)
       (bytes bytes (cdr bytes)))
      ((null bytes) 
       (let ((max (expt 256 i)))
	 (if (and signed (> integer (ash max -1)))
	     (- integer max)
	     integer)))
    (setf integer 
	  (logior integer 
		  (ash (car bytes) (* 8 i))))))

(defun unpack-bytes (buffer start size &optional signed)
  "Get an integer from the buffer."
  (let ((bytes (loop for i below size collect 
		    (elt buffer (+ start i)))))
    (merge-bytes bytes signed)))

(defun round-offset (offset packing)
  "Round the offset to the nearest packing boundary"
  (let ((i (mod offset packing)))
    (if (zerop i)
	offset
	(+ offset (- packing i)))))

;; ----------------- basic type defintions follow ----------------

(%define-type :uint8 
	      #'pack-uint8
	      #'unpack-uint8
	      1)

(%define-type :char 
	      (lambda (char buffer start)
		(pack-bytes (list (char-code char)) buffer start))
	      (lambda (buffer start)
		(let ((bytes (unpack-bytes buffer start 1)))
		  (code-char (car bytes))))
	      1)

(%define-type :wchar 
	      (lambda (char buffer start)
		(pack-bytes (bytes (char-code char) 2) buffer start))
	      (lambda (buffer start)
		(let ((code (unpack-bytes buffer start 2)))
		  (code-char code)))
	      2)

(%define-type :uint16
	      (lambda (uint16 buffer start)
		(pack-bytes (bytes uint16 2) buffer start))
	      (lambda (buffer start)
		(unpack-bytes buffer start 2))
	      2)

(%define-alias :ushort :uint16)

(%define-type :uint32 
	      (lambda (uint32 buffer start)
		(pack-bytes (bytes uint32 4) buffer start))
	      (lambda (buffer start)
		(unpack-bytes buffer start 4))
	      4)

(%define-alias :uint :uint32)

(%define-type :uint64 
	      (lambda (uint64 buffer start)
		(pack-bytes (bytes uint64 8) buffer start))
	      (lambda (buffer start)
		(unpack-bytes buffer start 8))
	      8)

(%define-alias :ulong :uint64)

(%define-type :int8 
	      (lambda (int8 buffer start)
		(pack-bytes (bytes int8 1) buffer start))
	      (lambda (buffer start)
		(unpack-bytes buffer start 1 t))
	      1)

(%define-alias :schar :int8)

(%define-type :int16
	      (lambda (int16 buffer start)
		(pack-bytes (bytes int16 2) buffer start))
	      (lambda (buffer start)
		(unpack-bytes buffer start 2 t))
	      2)

(%define-alias :short :int16)

(%define-type :int32 
	      (lambda (int32 buffer start)
		(pack-bytes (bytes int32 4) buffer start))
	      (lambda (buffer start)
		(unpack-bytes buffer start 4 t))
	      4)

(%define-alias :int :int32)

(%define-type :int64 
	      (lambda (int64 buffer start)
		(pack-bytes (bytes int64 8) buffer start))
	      (lambda (buffer start)
		(unpack-bytes buffer start 8 t))
	      8)

(%define-alias :long :int64)

;; these requre the ieee-floats package
(%define-type :float
	      (lambda (float buffer start)
		(let ((bytes (bytes (ieee-floats:encode-float32 float))))
		  (pack-bytes bytes buffer start)))
	      (lambda (buffer start)
		(let ((bytes (unpack-bytes buffer start 4)))
		  (ieee-floats:decode-float32 bytes)))
	      4)

(%define-type :double
	      (lambda (double buffer start)
		(let ((bytes (bytes (ieee-floats:encode-float64 double) 8)))
		  (pack-bytes bytes buffer start)))
	      (lambda (buffer start)
		(let ((bytes (unpack-bytes buffer start 8)))
		  (ieee-floats:decode-float64 bytes)))
	      8)

(%define-type :string 
	      (lambda (string buffer start)
		(pack-string string (1+ (length string)) buffer start))
	      (lambda (buffer start)
		(unpack-string 1 buffer start))
	      1)

(defun pack-type (object type buffer start)
  "Pack an object into the buffer."
  (when (symbolp type)
    (setf type (get-type type)))
  (let ((packer (type-packer type)))
    (funcall packer object buffer start)))

(defun unpack-type (type buffer start)
  "Unpack an objcet from the buffer."
  (when (symbolp type)
    (setf type (get-type type)))
  (let ((unpacker (type-unpacker type)))
    (funcall unpacker buffer start)))


;; ------ arrays -----------
  
(defun pack-array (array type buffer start)
  "Pack an array of object into the buffer."
  (let ((size (type-size type)))
    (dotimes (i (length array))
      (pack-type (elt array i) type buffer (+ start (* i size))))))

(defun unpack-array (length type buffer start)
  "Extract an array of objects from the buffer."
  (let ((array (make-array length))
	(size (type-size type)))
    (dotimes (i length)
      (setf (elt array i)
	    (unpack-type type buffer (+ start (* i size)))))
    array))


(defun pack-string (string length buffer start)
  "Pack a string into the buffer."
  (let ((array (make-array length)))
    (dotimes (i length)
      (if (< i (length string))
	  (setf (elt array i) (char-code (char string i)))
	  (setf (elt array i) 0)))
    (pack-array array
		:uint8
		buffer
		start)))

(defun unpack-string (length buffer start)
  "Unpack a string from the buffer."
  (let ((bytes (unpack-array length :uint8 buffer start)))
    (with-output-to-string (s)
      (block extract-string
	(dotimes (i (length bytes))
	  (let ((code (elt bytes i)))
	    (if (zerop code)
		(return-from extract-string)
		(princ (code-char code) s))))))))

;; --------- packets / structures --------------------------


(defparameter *default-packing* 4
  "Default packing boundary.")

(defun %define-packet-type (name slots &key packing size)
  "Define a new composite type.

NAME is any symbol to refer to the new type.

SLOTS is a list of (SLOT-NAME SLOT-TYPE &rest SLOT-OPTIONS) where SLOT-NAME 
is a symbol for access to the CLOS object's slot. SLOT-TYPE is either a symbol
for the slot's type or (SLOT-TYPE LENGTH) for an array of objects.

Set PACKING to an integer to use a non-default packing width.

Set SIZE to set the total type size. This cannot be smaller than the minumum required buffer size."
  (unless packing 
    (setf packing *default-packing*))
  ;; first need to go through the slots and find the starting offsets for each slot and total packet size
  (let ((real-slots nil)
	(offset 0))
    (dolist (slot slots)
      (destructuring-bind (slot-name slot-type &rest slot-options) slot
	(declare (ignore slot-options))
	(let ((length (if (listp slot-type)
			  (second slot-type)
			  1))
	      (arrayp (listp slot-type))
	      (slot-type (if (listp slot-type)
			     (first slot-type)
			     slot-type)))

	  (push (list slot-name slot-type arrayp length offset)
		real-slots)
	  
	  (setf offset 
		(round-offset (+ offset (* (type-size slot-type) length))
			      packing)))))
    (cond
      ((not size)
       (setf size offset))
      ((< size offset)
       (error "Packet size ~S is smaller than total packet size ~S" size offset)))

    ;; we have to reverse them becuase they were pushed on so are in reverse order
    (setf real-slots (nreverse real-slots))

    (%define-type name 
		  (lambda (object buffer start)
		    (dolist (slot real-slots)
		      (destructuring-bind (slot-name slot-type arrayp length offset) slot
			(let ((value (slot-value object slot-name)))
			  (cond
			    ((not arrayp)
			     (pack-type value  slot-type buffer (+ start offset)))
			    ((stringp value)
			     (pack-string value length buffer (+ start offset)))
			    ((<= (length value) length)
			     (pack-array value slot-type buffer (+ start offset)))
			    (t (error "Array for slot ~S is too large" slot-name))))))
		    buffer)
		(lambda (buffer start)
		  (let ((object (make-instance name)))
		    (dolist (slot real-slots)
		      (destructuring-bind (slot-name slot-type arrayp length offset) slot
			(cond
			  ((not arrayp)
			   (setf (slot-value object slot-name)
				 (unpack-type slot-type buffer (+ start offset))))
			  ((eq slot-type :string)
			   (setf (slot-value object slot-name)
				 (unpack-string length buffer (+ start offset))))
			  (t
			   (setf (slot-value object slot-name)
				 (unpack-array length slot-type buffer (+ start offset)))))))

		    object))
		size)))

(defmacro defpacket (name slots &rest options)
  "Macro to simplify defining the CLOS class and packet type.

SLOTS should be a list of format (SLOT-NAME SLOT-TYPE &rest SLOT-OPTIONS). 

SLOT-NAME should be a symbol used for refering to the CLOS class slot. 

SLOT-TYPE should be a symbol refering to a previously defined packet type (forward references 
are forbidden because we need to know the total packet size at definition time). 
Use (SLOT-TYPE LENGTH) instead for arrays.

SLOT-OPTIONS are passed to the defclass slot specifier.

OPTIONS can contain (:packing PACKING) and (:size SIZE). These are used to 
set the packing width and total packet size. All other options are passed to defclass."
  `(progn
     ;; define the clos class
     (defclass ,name ()
       ,(mapcar (lambda (slot)
		  (destructuring-bind (slot-name slot-type &rest slot-options) slot
		    (declare (ignore slot-type))
		    `(,slot-name ,@slot-options)))
		slots)
       ,@(mapcan (lambda (option)
		   (unless (member (car option) '(:packing :size))
		     (list option)))
		 options))
     ;; define the packet type
     (%define-packet-type ',name
			  (list ,@(mapcar (lambda (slot)
					    (destructuring-bind (slot-name slot-type &rest slot-options) slot
					      (declare (ignore slot-options))
					      (if (listp slot-type)
						  `(list ',slot-name (list ',(car slot-type) ,(cadr slot-type)))
						  `(list ',slot-name ',slot-type))))
					  slots))
			  :packing ,(cadr (assoc :packing options))
			  :size ,(cadr (assoc :size options)))
     nil))


(defun pack (object type)
  "Make a packet buffer from the initial object."
  (let ((buffer (make-array (type-size type) 
			    :element-type '(unsigned-byte 8)
			    :initial-element 0)))
    (pack-type object type buffer 0)
    buffer))

(defun unpack (buffer type)
  "Unpack a buffer into an object."
  (unpack-type type buffer 0))

