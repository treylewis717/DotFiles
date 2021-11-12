(setq doom-font (font-spec :family "RobotoMono Nerd Font Mono" :size 11)
      doom-big-font (font-spec :family "RobotoMono Nerd Font Mono" :size 11))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))
(setq global-prettify-symbols-mode t)

;; (require 'exwm)
;; (require 'exwm-config)
;; (setq exwm-input-global-keys
      ;; `(([?\s-q] . exwm-reset)
        ;; ,@(mapcar (lambda (i)
                    ;; `(,(kbd (format "s-%d" i)) .
                      ;; (lambda ()
                        ;; (interactive)
                        ;; (exwm-workspace-switch-create ,i))))
                  ;; (number-sequence 0 9))
        ;; ([?\s-<return>] . (lambda ()
                            ;; (interactive)
                            ;; (start-process "" nil "/usr/bin/alacritty -e fish"))))

;; (define-key exwm-mode-map [?\s-t] 'exwm-floating-toggle-floating)

;; (require 'exwm-systemtray)
;; (exwm-systemtray-enable)

;; (setq exwm-workspace-number 6)

;; (exwm-enable)

(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode))
                              auto-mode-alist))

(after! neotree
  (setq neo-smart-open t
        neo-window-fixed-size nil))

(after! doom-themes
  (setq doom-neotree-enable-variable-pitch t))

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (ffip-project-root))
        (file-name (buffer-file-name)))
    (if project-dir
        (progn
          (neotree-dir project-dir)
          (neotree-find file-name))
      (message "Could not find git project root."))))

(map! :leader
      :desc "Toggle neotree file viewer" "t n" #'neotree-toggle
      :desc "Put neofetch in project root dir" "d p" #'neotree-project-dir
      :desc "Open directory in neotree" "d n" #'neotree-dir)
(after! doom-dashboard-init-h
  (neotree))

(add-hook! '+doom-dashboard-functions :append
           (insert "\n" (+doom-dashboard--center +doom-dashboard--width "Yay evil!")))
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)

(setq doom-theme 'doom-one)

(setq display-line-numbers-type t)

(require 'elcord)
(elcord-mode)
(setq elcord-use-major-mode-as-main-icon 't)
;; (setq elcord-show-small-icon 'nil)

(after! rustic
  (setq rustic-lsp-server 'rust-analyzer))

(after! org
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-directory "~/Org/"
        org-agenda-files '("~/Org/agenda.org")
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-ellipsis " ▼ "
        org-log-done 'time
        org-journal-dir "~/Org/journal/"
        org-journal-date-format "%B %d, %Y (%A) "
        org-journal-file-format "%Y-%m-%d.org"
        org-hide-emphasis-markers t
        org-link-abbrev-alist
          '(("google" . "http://www.google.com/search?q=")
            ("arch-wiki" . "https://wiki.archlinux.org/index.php/")
            ("ddg" . "https://duckduckgo.com/?q=")
            ("wiki" . "https://en.wikipedia.org/wiki/"))))



(fset 'xml-mode 'nxml-mode)
(add-to-list 'auto-mode-alist '("\\.ui\\'" . nxml-mode))

(evil-define-key 'normal dired-mode-map
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-open-file)
(evil-define-key 'normal peep-dired-mode-map
  (kbd "j") 'peep-dired-next-file
  (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(setq dired-open-extensions '(("gif" . "sxiv")
                              ("jpg" . "sxiv")
                              ("png" . "sxiv")
                              ("mkv" . "mpv")
                              ("mp4" . "mpv")))

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(load "~/.config/doom/email")
(defun my-mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var))
                                                my-mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                             nil t nil nil (caar my-mu4e-account-alist))))
         (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))

(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)

(use-package emojify
  :hook (after-init . global-emojify-mode))
