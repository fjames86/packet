
PACKET
========

PACKET is a simple library for defining C-style structures and packing/unpacking them into Common Lisp arrays.

It is typically used for handling UDP packets, but could also be used to unpacking e.g. mmap'ed data files.

Packet type definitions
------------------------

Packet comes with some pre-defined intrinsic data types:
* 1,2,4 and 8 byte signed/unsigned integers
* 32/64-bit floats (encoding/decoding provided by IEEE-FLOATS package)
* characters and wide characters
* arrays of primitives, such as strings

Users should define their packet type using:

(defpacket name
  ((slot-name slot-type &rest slot-options)
   ...)
  &rest options)

This defines a defclass for the structure and defines a new packet type.

The slot-type can either be a symbol referring to a previously defined type (either primitive or a previous defpacket) 
or it can be a form (slot-type length) which means an array of length of slot-type objects.

The slot-options are passed into defclass slot specifier.

Options can include: 
(:packing <integer packing>) which sets the packing width for the slots
(:size <integer size>) which sets the total packet buffer size

All other options are passed as options to defclass.

Usage
---------

Make a packet buffer using:
(pack object type) 
This returns the buffer filled in with information.

Extract an object from a buffer using:
(unpack buffer type)

It's that simple. Have fun!




Frank James 
March 2014

