
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
		   #:unpack
		   #:type-size))

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
;; Strings (fixed-width buffers) are handled in a special (hacky) way
;; There should be a better way of doing it.
;;
;;
;; Note that we don't have to concern ourselves with pointers or anything like that.
;;
;; There is probably a better way of organising some of the code using generic functions,
;; particularly for packing/unpacking of the CLOS instances where it seems to make
;; the most sense. At the moment we generate a closure at definition time, but this could
;; in principle be done using a specialized generic function method.
;;
;;

;; alist of type definitions
(defvar *type-definitions* (make-hash-table)
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
  "Get the total size in bytes for this type."
  (when (symbolp type)
    (setf type (get-type type)))
  (fourth type))

(defun %define-alias (alias name)
  "Define an alias for a type. Allows refering to the same type by a different name."
  (let ((type (get-type name)))
    (%define-type alias (type-packer type) (type-unpacker type) (type-size type))))

(defun bytes (integer size)
  "Expand an INTEGER into its consituent number of SIZE bytes."
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
  "Convert a list of bytes into a signed/unsigned integer"
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

(defmacro define-type (name ((pobject pbuffer pstart) &body pbody) ((ubuffer ustart) &body ubody) size)
  `(progn
     (%define-type ',name 
				   (lambda (,pobject ,pbuffer ,pstart) ,@pbody)
				   (lambda (,ubuffer ,ustart) ,@ubody)
				   ,size)))

(defmacro define-alias (alias name)
  `(progn
     (%define-alias ',alias ',name)))

(define-type :uint8 
    ((uint buffer start) (setf (elt buffer start) uint))
  ((buffer start) (elt buffer start))
  1)

(define-type :char 
    ((char buffer start)
     (pack-bytes (list (char-code char)) buffer start))
  ((buffer start)
   (let ((bytes (unpack-bytes buffer start 1)))
     (code-char (car bytes))))
  1)

(define-type :wchar 
    ((char buffer start)
     (pack-bytes (bytes (char-code char) 2) buffer start))
  ((buffer start)
   (let ((code (unpack-bytes buffer start 2)))
     (code-char code)))
  2)

(define-type :uint16
    ((uint16 buffer start)
     (pack-bytes (bytes uint16 2) buffer start))
  ((buffer start)
   (unpack-bytes buffer start 2))
  2)

(define-alias :ushort :uint16)

(define-type :uint32 
    ((uint32 buffer start)
     (pack-bytes (bytes uint32 4) buffer start))
  ((buffer start)
   (unpack-bytes buffer start 4))
  4)

(define-alias :uint :uint32)

(define-type :uint64 
    ((uint64 buffer start)
     (pack-bytes (bytes uint64 8) buffer start))
  ((buffer start)
   (unpack-bytes buffer start 8))
  8)

(define-alias :ulong :uint64)

(define-type :int8 
    ((int8 buffer start)
     (pack-bytes (bytes int8 1) buffer start))
  ((buffer start)
   (unpack-bytes buffer start 1 t))
  1)

(define-alias :schar :int8)

(define-type :int16
    ((int16 buffer start)
     (pack-bytes (bytes int16 2) buffer start))
  ((buffer start)
   (unpack-bytes buffer start 2 t))
  2)

(define-alias :short :int16)

(define-type :int32 
    ((int32 buffer start)
     (pack-bytes (bytes int32 4) buffer start))
  ((buffer start)
   (unpack-bytes buffer start 4 t))
  4)

(define-alias :int :int32)

(define-type :int64 
    ((int64 buffer start)
     (pack-bytes (bytes int64 8) buffer start))
  ((buffer start)
   (unpack-bytes buffer start 8 t))
  8)

(define-alias :long :int64)

;; these require the ieee-floats package
(define-type :float
    ((float buffer start)
     (let ((bytes (bytes (ieee-floats:encode-float32 float) 4)))
       (pack-bytes bytes buffer start)))
  ((buffer start)
   (let ((bytes (unpack-bytes buffer start 4)))
     (ieee-floats:decode-float32 bytes)))
  4)

(define-type :double
    ((double buffer start)
     (let ((bytes (bytes (ieee-floats:encode-float64 double) 8)))
       (pack-bytes bytes buffer start)))
  ((buffer start)
   (let ((bytes (unpack-bytes buffer start 8)))
     (ieee-floats:decode-float64 bytes)))
  8)

;; booleans are really just uint32's
(define-type :bool
    ((bool buffer start)
     (pack-type (if bool 1 0) :uint32 buffer start))
  ((buffer start)
   (let ((bool (unpack-type :uint32 buffer start)))
     (not (= bool 0))))
  4)

;; This is a horrible hack. we need to have :string type defined so 
;; that we can compute the size of string-type slots, which are represented as arrays.
;; i.e. the size of (:string 12) should be 12.
;; We compute the size of arrays by multiplying the length of the array by the size of the base element.
;; We just alias :string to :uint8 so that we can get its size = 1.
;; the actual packing/unpacking is done by some special code in the closures
;; generated by %define-packet-type. There MUST be a better way of doing it!!!
(define-alias :string :uint8)

;; it'd be nicer if we could just define :string as a packet type, but this isn't possible.
;; maybe we need to be able to define array types?

;; -----------------------------------------------------------


(defun pack-type (object type buffer start)
  "Pack an object into the buffer."
  (when (symbolp type)
    (setf type (get-type type)))
  (unless type
    (error "Unknown type"))
  (let ((packer (type-packer type)))
    (funcall packer object buffer start)))

(defun unpack-type (type buffer start)
  "Unpack an objcet from the buffer."
  (when (symbolp type)
    (setf type (get-type type)))
  (unless type
    (error "Unknown type"))
  (let ((unpacker (type-unpacker type)))
    (funcall unpacker buffer start)))


;; ------------------------- arrays ------------------------------------

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

;; define some special functions for handling strings... can this be done better??
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

;; --------------------- packets / structures --------------------------


(defparameter *default-packing* 4
  "Default packing boundary.")

(defun compute-real-slots (slots packing &optional size)
  "Compute the offsets of each slot. We use this information in the closures
generated by DEFPACKET to pack/unpack the object."
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

		  ;; check it is a real type
		  (unless (get-type slot-type)
			(error "Packet type ~S not found." slot-type))

		  ;; check it it's an array that an integer length was provided
		  (when (and arrayp (not (integerp length)))
			(error "Array length ~S not an integer." length))
		  
		  ;; check it's not a string without length
		  (when (and (eq slot-type :string) (not arrayp))
			(error "Strings must be given an explicit length."))

		  ;; all ok, push the slot info onto the list
		  (push (list slot-name slot-type arrayp length offset)
				real-slots)

		  ;; compute the offset of the next slot
		  (let ((type-size (type-size slot-type)))
			(setf offset 
				  (round-offset (+ offset (* type-size length))
								packing))))))

    ;; check the size, if given
    (when (and size (< size offset))
      (error "Explicit packet size ~S smaller than minumum packet size ~S."
			 size offset))
    
    ;; we have to reverse them becuase they were pushed on so are in reverse order
    (values (nreverse real-slots)
			(if size size offset))))

;; we define some functions to pack/unpack CLOS objects.
;; It is probably possible to convert this to a generic function and have
;; defpacket generate a defmethod ??? That should be more efficient/nicer?

(defun pack-object (object slots buffer start)
  "Pack an OBJECT specified by SLOTS into the BUFFER starting at offset START."
  (unless object
    (return-from pack-object buffer))
  
  ;; iterate over the structure slots
  (dolist (slot slots)
    (destructuring-bind (slot-name slot-type arrayp length offset) slot
      (let ((value (slot-value object slot-name)))
		(cond
		  ((not arrayp)
		   (pack-type value slot-type buffer (+ start offset)))
		  ((null value)
		   ;; ignore arrays given as nil, this means an empty array
		   nil)
		  ((eq slot-type :string)
		   ;; special case handling for strings. there must be a better way of doing this!!!
		   (if (stringp value)
			   (pack-string value length buffer (+ start offset))
			   (error "Cannot pack ~S as :string for slot ~S" value slot-name)))
		  ((> (length value) length)
		   (error "Array for slot ~S is too large" slot-name))
		  (t 
		   (pack-array value slot-type buffer (+ start offset)))))))
  buffer)

(defun unpack-object (object slots buffer start)
  "Unpack the SLOTS of OBJECT from BUFFER starting at offset START."
  (dolist (slot slots)
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
  object)

(defmacro defpacket (name slots &rest options)
  "Macro to define the CLOS class and packet type.

SLOTS should be a list of format (SLOT-NAME SLOT-TYPE &rest SLOT-OPTIONS). 

SLOT-NAME should be a symbol used for refering to the CLOS class slot. 

SLOT-TYPE should be a symbol refering to a previously defined packet type (forward references 
are forbidden because we need to know the total packet size at definition time). 
Use (SLOT-TYPE LENGTH) for arrays.

SLOT-OPTIONS are passed to the corresponding defclass slot specifier.

OPTIONS can contain (:packing PACKING) and (:size SIZE). These are used to 
set the packing width and total packet size. If not specified PACKING defaults to *DEFAULT-PACKING*,
SIZE defaults to the total size of packet type. If specified, SIZE may not be less than this, but 
can be more, to allow for unused space at the end of the structure.

All other options are passed to defclass."
  (let ((gpacking (gensym "PACKING"))
		(gsize (gensym "SIZE"))
		(gslots (gensym "SLOTS")))
    `(progn
       ;; define the CLOS class
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
       
       ;; define the new packet type
       (let ((,gpacking ,(let ((p (cadr (assoc :packing options))))
							  (if p p '*default-packing*))))
		 (multiple-value-bind (,gslots ,gsize)
			 (compute-real-slots
			  (list ,@(mapcar (lambda (slot)
								(destructuring-bind (slot-name slot-type &rest slot-options) slot
								  (declare (ignore slot-options))
								  (if (listp slot-type)
									  `(list ',slot-name (list ',(car slot-type) ,(cadr slot-type)))
									  `(list ',slot-name ',slot-type))))
							  slots))
			  ,gpacking
			  ,(cadr (assoc :size options)))
		   (%define-type ',name
						 (lambda (object buffer start)
						   (pack-object object ,gslots buffer start))
						 (lambda (buffer start)
						   (unpack-object (make-instance ',name) ,gslots buffer start))
						 ,gsize)))
       ',name)))


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

