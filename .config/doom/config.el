(load "~/.config/doom/calendar")

(setq user-full-name "Trey Lewis"
      user-mail-address "treylewis717@tutanota.com")

(evil-define-key 'normal dired-mode-map
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-open-file)
(evil-define-key 'normal peep-dired-mode-map
  (kbd "j") 'peep-dired-next-file
  (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)
(setq dired-open-extensions '(("gif" . "sxiv")
                              ("jpg" . "sxiv")
                              ("png" . "sxiv")
                              ("mkv" . "mpv")
                              ("mp4" . "mpv")))

(elcord-mode)
(setq elcord-use-major-mode-as-main-icon 't)
(setq elcord-display-elasped nil)
(setq elcord-quiet 't)
(setq elcord-show-small-icon 'nil)



(after! org
  (setq org-directory "~/Org/"
        org-agenda-files '("~/Org/agenda.org")
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-ellipsis " ▼ "
        org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆")
        org-superstar-item-bullet-alist '((?+ . ?➤) (?- . ?✦)) ; changes +/- symbols in item lists
        org-log-done 'time
        org-journal-dir "~/Org/journal/"
        org-journal-date-format "%B %d, %Y (%A) "
        org-journal-file-format "%Y-%m-%d.org"
        org-hide-emphasis-markers t
        org-link-abbrev-alist
          '(("google" . "http://www.google.com/search?q=")
            ("arch-wiki" . "https://wiki.archlinux.org/index.php/")
            ("ddg" . "https://duckduckgo.com/?q=")
            ("wiki" . "https://en.wikipedia.org/wiki/")))
  (setq org-src-preserve-indentation nil
       org-src-tab-acts-natively t
       org-edit-src-content-indentation 0)
  (setq org-blank-before-new-entry (quote ((heading . nil)
                                          (plain-list-item . nil))))
  (setq org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-confirm-babel-evaluate nil
      org-edit-src-content-indentation 0))

(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode))
                              auto-mode-alist))

(after! rustic
  (setq rustic-babel-format-src-block nil
        rustic-format-trigger nil))

(fset 'xml-mode 'nxml-mode)
(add-to-list 'auto-mode-alist '("\\.ui\\'" . nxml-mode))

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

;;(after! neotree
;;  (setq neo-smart-open t
;;        neo-window-fixed-size nil))
;;
;;(after! doom-themes
;;  (setq doom-neotree-enable-variable-pitch t))
;;
;;(defun neotree-project-dir ()
;;  "Open NeoTree using the git root."
;;  (interactive)
;;  (let ((project-dir (ffip-project-root))
;;        (file-name (buffer-file-name)))
;;    (if project-dir
;;        (progn
;;          (neotree-dir project-dir)
;;          (neotree-find file-name))
;;      (message "Could not find git project root."))))
;;
;;(map! :leader
;;      :desc "Toggle neotree file viewer" "t n" #'neotree-toggle
;;      :desc "Put neofetch in project root dir" "d p" #'neotree-project-dir
;;      :desc "Open directory in neotree" "d n" #'neotree-dir)
;;(neotree)

(setq doom-font (font-spec :family "RobotoMono Nerd Font Mono" :size 11)
      doom-big-font (font-spec :family "RobotoMono Nerd Font Mono" :size 11))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))
(setq global-prettify-symbols-mode t)

(setq doom-theme 'doom-one)

(setq display-line-numbers-type t)

(use-package emojify
  :hook (after-init . global-emojify-mode))
