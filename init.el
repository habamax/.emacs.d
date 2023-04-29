;;; init.el --- emacs init file -*- lexical-binding: t; -*-

;;; Commentary:

;; habamax's personal Emacs configuration

;;; Code:

;;; How long it took this time?
(add-hook 'emacs-startup-hook (lambda () (message "%s" (emacs-init-time))))

;;; disable gc for init
(setq gc-cons-threshold 64000000)
(add-hook
 'after-init-hook
 (lambda () (setq gc-cons-threshold (default-value 'gc-cons-threshold))))

;;; emacsclient to focus new frame
(add-hook
 'server-after-make-frame-hook
 (lambda () (select-frame-set-input-focus (selected-frame))))

(setq user-full-name "Maxim Kim"
      user-mail-address "habamax@gmail.com"
      send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-stream-type 'starttls
      smtpmail-smtp-service 587)

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(if +IS-WINDOWS+
    (let ((fonts '(("JetBrains Mono NL" . "JetBrains Mono NL-12")
                   ("Cascadia Mono SemiLight" . "Cascadia Mono SemiLight-12")
                   ("Consolas" . "Consolas-12"))))
      (cl-dolist (fnt fonts)
        (when (find-font (font-spec :name (car fnt)))
          (add-to-list 'default-frame-alist `(font . ,(cdr fnt)))
          (set-face-attribute 'fixed-pitch nil :font (cdr fnt))
          (set-face-attribute 'fixed-pitch-serif nil :font (cdr fnt))
          (cl-return))))
  (add-to-list 'default-frame-alist '(font . "Monospace-16")))

(when +IS-WINDOWS+
  (set-language-environment 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8-unix))

(setq default-input-method 'russian-computer)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq ring-bell-function #'ignore)
(setq scroll-error-top-bottom t)
(setq disabled-command-function nil)
(setq suggest-key-bindings t)
(setq-default fill-column 80)
(setq-default display-fill-column-indicator-character ?╎)
(setq-default indent-tabs-mode nil)
(setq-default isearch-lazy-count t)
(setq-default abbrev-mode t)
(setq-default line-number-mode t)
(setq describe-bindings-outline t)
(setq set-mark-command-repeat-pop t)

;;; Simple HTML renderer to use default font.
(setq shr-use-fonts nil)

;;; ripgrep as grep
(setq grep-command "rg -nS --no-heading "
      grep-use-null-device nil)

;;; dired
(setq ls-lisp-dirs-first t)
(setq dired-listing-switches "-lah --group-directories-first"
      dired-dwim-target t)

;;; Store all backup and autosave files in the tmp dir
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq vc-follow-symlinks t)

(setq tab-bar-show 1)

(electric-pair-mode t)
(delete-selection-mode t)
(save-place-mode 1)
(savehist-mode 1)
(recentf-mode 1)
(repeat-mode t)
(winner-mode t)
(fido-vertical-mode 1)

(setq completion-auto-help 'always
      completion-show-help nil)

(if (window-system)
    (cd "~/"))

;;; use packages
(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

(setq-default
 package-native-compile t
 use-package-always-ensure t
 use-package-always-defer t
 use-package-enable-imenu-support t)

(require 'use-package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") 'append)

(use-package windows
  :ensure nil
  :bind (("<f8>" . window-toggle-side-windows)
         ("C-c w" . winner-undo)
         ("C-c W" . winner-redo)
         ("M-<right>" . windmove-swap-states-right)
         ("M-<left>" . windmove-swap-states-left)
         ("M-<up>" . windmove-swap-states-up)
         ("M-<down>" . windmove-swap-states-down)
         :repeat-map habamax-other-frame-map
         ("o" . other-frame)
         :repeat-map habamax-winner-map
         ("w" . winner-undo)
         ("W" . winner-redo))
  :init
  (setq display-buffer-alist
        '(("\\*e?shell\\*"
           (display-buffer-in-side-window)
           (window-height . 0.3)
           (side . top)
           (slot . 0))
          ("\\*\\(grep\\|compilation\\|godot - .+\\)\\*"
           (display-buffer-in-side-window)
           (window-height . 0.3)
           (side . bottom)
           (slot . 0)))))

(use-package habamax
  :load-path "site-lisp"
  :bind (("M-;" . habamax/toggle-comment)
         ("M-o" . delete-blank-lines)
         ("M-n" . habamax/move-line-down)
         ("M-p" . habamax/move-line-up)
         ("C-w" . habamax/kill-region)
         ("M-w" . habamax/kill-ring-save)
         ("C-x C-r" . habamax/recentf-open)
         ("C-c b" . habamax/next-buffer-like-this)
         ("C-c B" . habamax/previous-buffer-like-this)
         ("C-c i d" . habamax/insert-current-date)
         ([remap list-buffers] . ibuffer)
         ("C-c d" . habamax/duplicate-line)
         ("M-s g" . habamax/grep-current-word)
         ("M-s t" . habamax/grep-todo)
         ("C-c m" . imenu)
         ("C-c o i" . habamax/init-file)
         ("C-c t n" . display-line-numbers-mode)
         ("C-c t SPC" . whitespace-mode)
         ("C-c t s" . flyspell-mode)
         ("C-c t l" . hl-line-mode)
         ("C-c t l" . hl-line-mode)
         ("C-c t f" . display-fill-column-indicator-mode)
         ("C-c t b" . habamax/toggle-bg)
         ("C-c t v" . visible-mode)
         :repeat-map habamax-duplicate-line-repeat-map
         ("d" . habamax/duplicate-line)
         :repeat-map habamax-buffers-like-this-map
         ("b" . habamax/next-buffer-like-this)
         ("B" . habamax/previous-buffer-like-this))
  :config
  (defun habamax/init-file ()
    (interactive)
    (find-file user-init-file)))

(use-package habamax-dev
  :load-path "site-lisp"
  :commands (habamax-dev/run-c-file
             habamax-dev/run-python-file
             habamax-dev/run-cargo)
  :init
  (add-hook 'c-mode-hook
            (lambda ()
              (local-set-key [f5] 'habamax-dev/run-c-file)
              (c-set-style "linux")
              (setq-local c-basic-offset 4)
              (c-toggle-comment-style -1)))
  (add-hook 'python-mode-hook
            (lambda () (local-set-key [f5] 'habamax-dev/run-python-file))))

(use-package org
  :ensure nil
  :bind (("C-c o c" . org-capture)
         ("C-c o a" . org-agenda)
         ("C-c o n" . org/notes)
         ("C-c o t" . org/todo)
         ("C-c o s" . org/insert-screenshot)
         :repeat-map habamax-org-map
         ("t" . org-todo))
  :mode (("\\.org$" . org-mode))
  :config
  (require 'habamax-org)
  (setq org-directory (or (getenv "DOCS") "~/docs"))
  (setq org-agenda-files '("todo.org" "notes.org"))
  (setq org-refile-use-outline-path 'file
        org-refile-targets '((org-agenda-files :maxlevel . 1)))
  (setq org-todo-keywords '((sequence "TODO" "DELEGATED" "WAITING"
                                      "|" "CANCELLED" "DONE")))
  (setq org-todo-keyword-faces
        '(("CANCELLED" . font-lock-type-face)
          ("WAITING" . font-lock-function-name-face)
          ("DELEGATED" . font-lock-preprocessor-face)))
  (setq org-capture-templates
      '(("t" "Todo" entry (file "todo.org")
         "* TODO %?\n%U" :empty-lines 1 :prepend t)
        ("T" "Todo with link" entry (file "todo.org")
         "* TODO %?\n%U\n\n%i\n%a" :empty-lines 1 :prepend t)
        ("n" "Note" entry (file "notes.org")
         "* %?\n%T\n%i\n" :empty-lines 1 :prepend t)
        ("N" "Note with link" entry (file "notes.org")
         "* %?\n%T\n%i\n%a" :empty-lines 1 :prepend t)
        ("m" "Meeting notes" entry (file "notes.org")
         "* Meeting Notes\n%T\n\n*Attendees:*\n\n%?\n\n*Status:*\n\n"
         :empty-lines 1 :prepend t)))
  (setq org-export-with-sub-superscripts '{}
        org-export-headline-levels 5
        org-export-with-email t)
  (setq org-html-doctype "html5"
        org-html-html5-fancy t
        org-html-checkbox-type 'html
        org-html-validation-link nil)
  (setq org-html-head-include-default-style nil
        org-html-htmlize-output-type 'css)
  (setq org-html-style
        (concat "<style type=\"text/css\">\n"
                (with-temp-buffer
                  (insert-file-contents (concat user-emacs-directory "org/org.css"))
                  (buffer-string))
                "</style>\n")))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package dictionary
  :ensure nil
  :commands dictionary-lookup-definition
  :bind ("C-c l" . dictionary-lookup-definition)
  :config
  (setq dictionary-server "dict.org"))

(use-package whitespace
  :ensure nil
  :commands (whitespace-mode)
  :config
  (setq whitespace-style '(face trailing tabs spaces tab-mark space-mark))
  (setq whitespace-display-mappings '((tab-mark 9 [8250 9])
                                      (space-mark 32 [183][46])
                                      (space-mark 160 [164][95]))))

(use-package magit
  :commands (magit-status)
  :bind ("C-x g" . magit-file-dispatch))

(use-package markdown-mode
  :mode "\\.txt$"
  :bind (:map markdown-mode-map
              ("M-n" . nil)
              ("M-p" . nil))
  :config
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-unordered-list-item-prefix "  - ")
  (setq markdown-asymmetric-header t)
  (setq markdown-command
        "pandoc -s -M fontsize=18pt -M maxwidth=60em --highlight-style tango"))

(use-package erc
  :ensure nil
  :commands erc
  :config
  (when (not +IS-WINDOWS+) (erc-notifications-mode t))
  (setq erc-nick '("habamax" "mxmkm")
        erc-track-minor-mode t
        erc-hide-list '("JOIN" "PART" "QUIT")
        erc-join-buffer 'bury
        erc-autojoin-channels-alist '(("Libera.Chat"
                                       "#emacs" "#vim" "#python" "#zig"))
        erc-server-reconnect-attempts 5
        erc-server-reconnect-timeout 3)
  (setq erc-kill-queries-on-quit t
        erc-kill-server-buffer-on-quit t)
  (setq erc-prompt-for-password nil
        erc-prompt-for-nickserv-password nil)
  (defun erc/emacs-vim ()
    (interactive)
    (delete-other-windows)
    (switch-to-buffer "#emacs")
    (switch-to-buffer-other-window "#vim")
    (other-window 1)))

(use-package erc-hl-nicks
  :after erc
  :config
  (erc-hl-nicks-force-nick-face
   "habamax" (face-attribute 'font-lock-constant-face :foreground)))

(use-package webpaste
  :commands (webpaste-paste-buffer webpaste-paste-region))

(use-package rainbow-mode
  :commands (rainbow-mode))

(use-package lorem-ipsum
  :commands (lorem-ipsum-insert-sentences lorem-ipsum-insert-paragraphs))

(use-package gdscript-mode)

(use-package zig-mode)

(use-package notmuch
  :ensure nil
  :if (executable-find "notmuch")
  :commands (notmuch notmuch-sync)
  :config
  (defun notmuch-sync ()
    (interactive)
    (when (executable-find "mbsync")
      (compile "mbsync -a && notmuch new")))
  (setq notmuch-show-logo nil
        notmuch-hello-auto-refresh t
        notmuch-hello-recent-searches-max 20
        notmuch-hello-sections '(notmuch-hello-insert-saved-searches
                                 notmuch-hello-insert-alltags)
        notmuch-show-all-tags-list t)
  (setq notmuch-search-oldest-first nil
        notmuch-show-empty-saved-searches t
        notmuch-saved-searches
        `(( :name "📥 Inbox"
            :query "folder:/Inbox/ or tag:inbox"
            :sort-order newest-first
            :key ,(kbd "i"))
          ( :name "💬 Unread (inbox)"
            :query "tag:unread and folder:/Inbox/"
            :sort-order newest-first
            :key ,(kbd "u"))
          ( :name "📚 All"
            :query "folder:/Archive/"
            :sort-order newest-first
            :key ,(kbd "a"))
          ( :name "💩 Trash"
            :query "folder:/Trash/"
            :sort-order newest-first
            :key ,(kbd "t"))
          ( :name "📨 Sent"
            :query "folder:/Sent/"
            :sort-order newest-first
            :key ,(kbd "s"))
          ( :name "📝 Drafts"
            :query "folder:/Drafts/"
            :sort-order newest-first
            :key ,(kbd "d")))))

(use-package emms
  :commands emms
  :config
  (emms-all)
  (setq emms-player-list '(emms-player-mpv emms-player-vlc)
        emms-info-functions '(emms-info-native))
  (setq emms-source-file-default-directory "~/Music/"))

;;; init.el ends here
