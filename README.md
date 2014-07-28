
PACKET
========

PACKET is a simple library for defining C-style structures and packing/unpacking them into Common Lisp arrays.

It is typically used for handling UDP packets, but could also be used for unpacking e.g. mmap'ed data files.

Packet type definitions
------------------------

Packet comes with some pre-defined intrinsic data types:
* 8, 16, 32 and 64 bit signed/unsigned integers (:uint8, :uint16, :uint32, :uint64 ...)
* 32/64-bit floats (encoding/decoding provided by IEEE-FLOATS package) 
* characters and wide characters 
* arrays of primitives, such as strings 

Users define their packet type and associated CLOS class using:

```
(defpacket name
  ((slot-name slot-type &rest slot-options)
   ...)
  &rest options)
```

This expands to a defclass for the structure and code for defining the new packet type.

The slot-type can either be a symbol referring to a previously defined type (either primitive or a previous defpacket) 
or it can be a form (slot-type length) which means an array of length of slot-type objects.

The slot-options are passed into the defclass slot specifier.

Options can include: 

```
(:packing <integer packing>) ;; sets the packing width for the slots
(:size <integer size>)       ;; sets the total packet buffer size
```

All other options are passed as options to defclass.

Usage
---------
Make a packet buffer using:
```
(pack object type) 
```
This returns the buffer filled in with information.

Extract an object from a buffer using:
```
(unpack buffer type)
```

Utilities 
-----------

Along with the serialization functions, there are also several other utilities which provide commonly required functionality:

* Bitwise flags:
```
(defflags *example-flags*
  ((:myflag1 0 "My flag1")
   (:myflag2 1 "My flag2")))
(pack-flags '(:myflag1 :myflag2) *example-flags*)
```

* Enums:
```
(defenum *example-enum*
  ((:a 0 "A")
   (:b 1 "B")))
(enum :a *example-enum*)
```

* Hexdump output:
```
(hd #(1 2 3 4))
```

Both the flags and enums expand to eval-when forms so that they can be computed at compile time e.g. like:
```
(defun test ()
  (list #.(enum :a *example-enum*)))
```

Example
----------

We are trying to send packets to a UDP server (written in C) expecting something like
```C
struct person_s {
  char name[20];
  uint32_t flags;
};

struct people_s {
  uint32_t magic;
  int32_t npeople;
  struct person_s people[20];
  uint32_t flags;
};
```

We define our packet type using:
```
(defconstant +magic-number+ #x1E2B3A4D)

(defpacket person-s
  ((name (:string 20) :initform "" :initarg :name)
   (flags :uint32 :initform 0 :initarg :flags))
  (:documentation "Person structure."))

(defpacket people-s 
  ((magic :uint32 :initform +magic-number+)
   (npeople :int32 :initform 0 :initarg :npeople)
   (people (person-s 20) :initform nil :initarg :people)
   (flags :uint32 :initform 0 :initarg :flags))
  (:documentation "Collection of people."))

;; pack an instance into a buffer
(pack (make-instance 'people-s 
                     :npeople 2
                     :people (list (make-instance 'person-s :name "frank" :flags 123)
		                   (make-instance 'person-s :name "james" :flags 321))
                     :flags 456)
      'people-s)
-> #(77 58 43 30 2 0 0 0 102 114 97 110 107 ... )

;; unpack a buffer into an instance
(unpack buffer 'people-s)
-> #<PEOPLE-S {24CB5061}>
```

See example.lisp for more simple usages.

TODO
-----

* Unions?
* Convert pack-object into a generic function 


Frank James 
March 2014

