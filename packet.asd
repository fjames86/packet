
(asdf:defsystem :packet
  :name "PACKET"
  :author "Frank James <frank.a.james@gmail.com>"
  :description "Simple binary serialization library."
  :license "BSD"
  :components
  ((:file "package")
   (:file "utils" :depends-on ("package"))
   (:file "packet" :depends-on ("utils")))
  :depends-on (:ieee-floats))

