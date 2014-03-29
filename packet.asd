
(asdf:defsystem :packet
  :name "PACKET"
  :author "Frank James <frank.a.james@gmail.com>"
  :description "Simple binary serialization library."
  :license "BSD"
  :components
  ((:file "packet"))
  :depends-on (:ieee-floats))

