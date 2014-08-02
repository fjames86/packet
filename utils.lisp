;;
;; Package for packing/unpacking strutures into/out of Lisp buffers. 
;; Can be used for e.g. UDP packets or mmap'ed data structures.
;; 
;; Copyright Frank James 
;; March 2014
;;


(in-package :packet)


(defun subseq* (sequence start &optional len)
  "Subsequence with length" 
  (subseq sequence start (when len (+ start len))))

(defun pad (array len)
  "Pad array with zeros if its too short" 
  (let* ((l (length array))
         (arr (make-array (max len l) :initial-element 0 :element-type '(unsigned-byte 8))))
    (dotimes (i (length arr))
      (when (< i l)
        (setf (elt arr i) (elt array i))))
    arr))

(defun pad* (array &optional (width 4))
  "Pad to a length multiple of WIDTH"
  (let* ((l (length array))
	 (m (mod l width)))
    (if (zerop m)
	array
	(pad array (+ l (- width m))))))

(defun hd (data)
  "Hexdump output"
  (let ((lbuff (make-array 16))
        (len (length data)))
    (labels ((pline (lbuff count)
               (dotimes (i count)
                 (format t " ~2,'0X" (svref lbuff i)))
               (dotimes (i (- 16 count))
                 (format t "   "))

               (format t " | ")
               (dotimes (i count)
                 (let ((char (code-char (svref lbuff i))))
                   (format t "~C" 
                           (if (graphic-char-p char) char #\.))))
               (terpri)))
      (do ((pos 0 (+ pos 16)))
          ((>= pos len))
        (let ((count (min 16 (- len pos))))
          (dotimes (i count)
            (setf (svref lbuff i) (elt data (+ pos i))))
          (format t "; ~8,'0X:  " pos)
          (pline lbuff count))))))


(defun usb8 (&rest sequences)
  "Make an (unsigned byte 8) vector from the sequences"
  (apply #'concatenate '(vector (unsigned-byte 8)) sequences))

(defun usb8* (&rest numbers)
  "Make an (unsigned-byte 8) vector from the numbers"
  (make-array (length numbers)
              :element-type '(unsigned-byte 8)
              :initial-contents numbers))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flags 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro defflags (name flags &optional documentation)
  "Macro to define a set of flags"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defparameter ,name
       (list ,@(mapcar (lambda (flag)
                         (destructuring-bind (n v &optional doc) flag
                           (let ((gv (gensym)))
                             `(let ((,gv ,v))
                                (list ',n (ash 1 ,gv) ,gv ,doc)))))
                       flags))
     ,documentation)))

(defun pack-flags (flag-names flags)
  "Combine flags"
  (let ((f 0))
    (dolist (flag-name flag-names)
      (let ((n (cadr (assoc flag-name flags))))
        (unless n (error "Flag ~S not found" flag-name))
        (setf f (logior f n))))
    f))

(defun unpack-flags (number flags)
  "Split the number into its flags."
  (let ((f nil)
        (num number))
    (dolist (flag flags)
      (let ((n (cadr flag)))
        (unless (zerop (logand number n))
          (push (car flag) f)
          (setf num (logand num (lognot n))))))
    (unless (zerop num)
      (warn "Input flags ~S remainder ~S" number num))
;;    (assert (zerop number))
    f))

(defun flag-p (number flag-name flags)
  (let ((flag (assoc flag-name flags)))
    (unless flag (error "No flag ~S" flag-name))
    (not (zerop (logand number (cadr flag))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enums 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         
(defmacro defenum (name enums)
  "Define a list of enums" 
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defparameter ,name
       (list ,@(let ((i 0)) 
                    (mapcar (lambda (enum)
                              (cond 
                                ((symbolp enum)
                                 (prog1 `(list ',enum ,i nil)
                                   (incf i)))
                                (t 
                                 (destructuring-bind (n v &optional doc) enum
                                   (prog1 `(list ',n ,v ,doc)
                                     (setf i (1+ v)))))))
                            enums))))))

(defun enum-p (number enum enums)
  (let ((e (assoc enum enums)))
    (unless e (error "No such enum ~S" enum))
    (= number (cadr e))))

(defun enum (enum enums)
  (let ((e (assoc enum enums)))
    (unless e (error "No such enum ~S" enum))
    (cadr e)))

(defun enum-id (number enums)
  (car (find number enums :key #'cadr)))






