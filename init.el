;;; -*- lexical-binding: t; -*-
;;; habamax: personal emacs configuration

;;; How long it took this time?
(add-hook 'emacs-startup-hook (lambda () (message "%s" (emacs-init-time))))

;;; disable gc for init
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (default-value 'gc-cons-threshold))))

;;; emacsclient to focus new frame
(add-hook 'server-after-make-frame-hook (lambda () (select-frame-set-input-focus (selected-frame))))

(setq user-full-name "Maxim Kim"
      user-mail-address "habamax@gmail.com")

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(if +IS-WINDOWS+
    (cond ((find-font (font-spec :name "JetBrains Mono NL"))
           (add-to-list 'default-frame-alist '(font . "JetBrains Mono NL-13")))
          ((find-font (font-spec :name "Consolas"))
           (add-to-list 'default-frame-alist '(font . "Consolas-13"))))
  (add-to-list 'default-frame-alist '(font . "Monospace-13")))

(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq default-input-method 'russian-computer)
(setq default-buffer-file-coding-system 'utf-8-unix)
(when +IS-WINDOWS+
  (setq default-process-coding-system '(utf-8-dos . cp1251-dos)))

(setq ring-bell-function #'ignore)
(setq scroll-error-top-bottom t)
(setq disabled-command-function nil)
(setq suggest-key-bindings t)
(setq-default fill-column 80)
(setq-default indent-tabs-mode nil)
(setq-default isearch-lazy-count t)

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
(repeat-mode t)
(winner-mode t)

;;; completions
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-url-at-point t)
(ido-mode 1)
(fido-mode 1)

(setq completion-auto-help 'always
      completion-show-help nil)

;; ;;; try when emacs 29 is out
;; (setq completion-styles '(flex)
;;       completion-auto-wrap t
;;       completion-auto-help 'always
;;       completion-auto-select 'second-tab
;;       completion-show-help nil
;;       completions-max-height 15
;;       max-mini-window-height 10)
;; (setq icomplete-show-matches-on-no-input t)
;; (icomplete-mode 1)



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

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") 'append)

(use-package habamax
  :load-path "lisp"
  :bind (("M-;" . habamax/toggle-comment)
         ("C-c M-;" . habamax/comment-fill-aligned)
         ("M-n" . habamax/move-line-down)
         ("M-p" . habamax/move-line-up)
         ("C-w" . habamax/kill-region)
         ("M-w" . habamax/kill-ring-save)
         ("C-c b" . habamax/next-buffer-like-this)
         ("C-c B" . habamax/previous-buffer-like-this)
         ("C-c id" . habamax/insert-current-date)
         ("C-c in" . habamax/insert-meeting-notes)
         ([remap list-buffers] . ibuffer)
         ("C-c d" . habamax/duplicate-line)
         ("M-s g" . habamax/grep-current-word)
         ("C-c m" . imenu)
         ("C-c w" . winner-undo)
         ("C-c W" . winner-redo)
         :repeat-map habamax-duplicate-line-repeat-map
         ("d" . habamax/duplicate-line)
         :repeat-map habamax-buffers-like-this-map
         ("b" . habamax/next-buffer-like-this)
         ("B" . habamax/previous-buffer-like-this)
         :repeat-map habamax-other-frame-map
         ("o" . other-frame)
         :repeat-map habamax-winner-map
         ("w" . winner-undo)
         ("W" . winner-redo)))

(use-package habamax-compile
  :load-path "lisp"
  :commands (habamax-compile/run-c-file
             habamax-compile/run-python-file
             habamax-compile/run-cargo)
  :init
  (add-hook 'c-mode-hook
            (lambda () (local-set-key [f5] 'habamax-compile/run-c-file)))
  (add-hook 'python-mode-hook
            (lambda () (local-set-key [f5] 'habamax-compile/run-python-file)))
  (add-hook 'rust-mode-hook
            (lambda () (local-set-key [f5] 'habamax-compile/run-cargo))))

(use-package whitespace
  :ensure nil
  :commands (whitespace-mode)
  :config
  (setq whitespace-style '(face trailing tabs spaces tab-mark space-mark))
  (setq whitespace-display-mappings '((tab-mark 9 [8250 9])
                                      (space-mark 32 [183][46])
                                      (space-mark 160 [164][95]))))

(use-package gnus
  :ensure nil
  :commands gnus
  :config
  (setq 
   user-full-name "Maxim Kim"
   user-mail-address "habamax@gmail.com"
   send-mail-function 'smtpmail-send-it
   smtpmail-smtp-server "smtp.gmail.com"
   smtpmail-stream-type 'starttls
   smtpmail-smtp-service 587
   gnus-select-method
   '(nnimap "gmail"
            (nnimap-address "imap.gmail.com")
            (nnimap-server-port 993)
            (nnimap-stream ssl)
            (nnmail-expiry-wait immediate))))

(use-package magit
  :commands (magit-status)
  :bind ("C-c g" . magit-file-dispatch))

(use-package markdown-mode
  :mode "\\.txt$"
  :bind (:map markdown-mode-map
              ("M-n" . nil)
              ("M-p" . nil))
  :config
  (setq markdown-unordered-list-item-prefix "  - ")
  (setq markdown-asymmetric-header t)
  (set-face-attribute 'markdown-header-face-1 nil :height 1.4)
  (set-face-attribute 'markdown-header-face-2 nil :height 1.2)
  (set-face-attribute 'markdown-header-face-3 nil :height 1.1))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)))

(use-package erc
  :ensure nil
  :commands erc
  :config
  (when (not +IS-WINDOWS+) (erc-notifications-mode t))
  (setq erc-nick '("habamax" "mxmkm")
        erc-track-minor-mode t
        erc-hide-list '("JOIN" "PART" "QUIT")
        erc-join-buffer 'bury
        erc-autojoin-channels-alist '(("Libera.Chat" "#emacs" "#vim" "#python"))
        erc-server-reconnect-attempts 5
        erc-server-reconnect-timeout 3)
  (setq erc-rename-buffers t
        erc-kill-buffer-on-part t
        erc-kill-queries-on-quit t
        erc-kill-server-buffer-on-quit t)
  (setq erc-prompt-for-password nil
        erc-prompt-for-nickserv-password nil))

(use-package erc-hl-nicks
  :after erc
  :config
  (erc-hl-nicks-force-nick-face "habamax" (face-attribute 'font-lock-constant-face :foreground)))

(use-package elfeed :commands elfeed)

(use-package webpaste
  :commands (webpaste-paste-buffer webpaste-paste-region))

(use-package lorem-ipsum
  :commands (lorem-ipsum-insert-sentences lorem-ipsum-insert-paragraphs))

(use-package gdscript-mode)

(use-package rust-mode)
