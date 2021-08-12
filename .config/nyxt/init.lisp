(in-package #:nyxt-user)

(dolist (file (list (nyxt-init-file "style.lisp")
                    (nyxt-init-file "keybinds.lisp")))
  (load file))
