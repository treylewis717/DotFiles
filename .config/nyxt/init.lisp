(in-package #:nyxt-user)

(dolist (file (list (nyxt-init-file "style.lisp")))
  (load file))
