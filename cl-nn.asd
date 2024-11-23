(asdf:defsystem #:cl-nn
  :pathname "src/"
  :serial t
  :depends-on (:sdl2 :sdl2-image :sdl2-ttf)
  :components ((:file "package")
               (:file "window")
               (:module "networks"
                :serial t
                :components ((:file "deltarule")
                             (:file "hopfield")))
               (:file "cl-nn")))
