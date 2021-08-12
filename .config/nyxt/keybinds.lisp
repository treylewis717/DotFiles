(in-package #:nyxt-user)

(define-configuration nyxt/web-mode:web-mode
    ((keymap-scheme (let ((scheme %slot-default%))
                      (keymap:define-key (gethash scheme:vi-normal scheme)
                        "space ." 'switch-buffer
                        "space" nil)
                      scheme))))
