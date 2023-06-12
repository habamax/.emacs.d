;;; init.el --- emacs init file -*- lexical-binding: t; -*-
;;; Commentary:
;; ¯\_(ツ)_/¯
;; Maxim Kim <habamax@gmail.com>
;;; Code:

;;; How long it took this time?
(add-hook 'emacs-startup-hook (lambda () (message "%s" (emacs-init-time))))

(set-language-environment 'utf-8)
(setq default-input-method 'russian-computer)
(setq default-buffer-file-coding-system 'utf-8-unix)

(setq custom-file (make-temp-file "custom-emacs"))

(setq user-full-name "Maxim Kim"
      user-mail-address "habamax@gmail.com"
      send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-stream-type 'starttls
      smtpmail-smtp-service 587)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq ring-bell-function #'ignore)
(setq scroll-error-top-bottom t)
(setq disabled-command-function nil)
(setq-default fill-column 80
              display-fill-column-indicator-character ?╎)
(setq-default indent-tabs-mode nil)
(setq-default isearch-lazy-count t)
(setq search-whitespace-regexp ".*?")
(setq-default line-number-mode t
              column-number-mode t)
(setq set-mark-command-repeat-pop t)
(setq show-paren-when-point-inside-paren t)
(setq tab-always-indent 'complete)
(setq compilation-scroll-output t)
(setq vc-follow-symlinks t)
(setq tab-bar-show 1)

(setq-default abbrev-mode t)
(quietly-read-abbrev-file (concat user-emacs-directory "abbrevs"))

;;; Simple HTML renderer to use default font.
(setq shr-use-fonts nil)
(setq shr-color-visible-luminance-min 78)

;;; ripgrep as grep
(setq grep-command "rg -nS --no-heading "
      grep-use-null-device nil)

;;; Store all backup and autosave files in the tmp dir
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq winner-dont-bind-my-keys t)
(winner-mode t)

(setq recentf-max-saved-items 1000)
(recentf-mode 1)

(electric-pair-mode t)
(delete-selection-mode t)
(save-place-mode 1)
(savehist-mode 1)
(repeat-mode t)

;; terminal truncate/wrap symbols
(set-display-table-slot standard-display-table
                        'truncation
                        (make-glyph-code ?… 'line-number-minor-tick))
(set-display-table-slot standard-display-table
                        'wrap
                        (make-glyph-code ?↩ 'line-number-minor-tick))

;; window layout
(setq display-buffer-alist
      '(("\\*\\(.*-\\)?e?shell\\*"
         (display-buffer-in-side-window)
         (window-height . 0.3)
         (side . bottom)
         (slot . -1))
        ("\\*\\(grep\\|compilation\\)\\*"
         (display-buffer-in-side-window)
         (window-height . 0.3)
         (side . bottom)
         (slot . 0))
        ("\\*Customize .*\\*"
         (display-buffer-in-side-window)
         (window-width . 0.3)
         (side . right)
         (slot . -1))))

;;; use packages
(require 'package)
(setq package-native-compile t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") 'append)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t
      use-package-always-defer t)

(use-package habamax
  :load-path "lisp"
  :bind
  (("C-c o i" . habamax/init)
   ("C-c o s" . habamax/secrets)
   ("C-c o r" . habamax/recentf)
   ("M-;" . habamax/toggle-comment)
   ("M-o" . delete-blank-lines)
   ("M-n" . habamax/move-line-down)
   ("M-p" . habamax/move-line-up)
   ("C-w" . habamax/kill-region)
   ("M-w" . habamax/kill-ring-save)
   ("C-c b" . habamax/next-buffer-like-this)
   ("C-c B" . habamax/previous-buffer-like-this)
   ([remap list-buffers] . ibuffer)
   ("C-c d" . habamax/duplicate-line)
   ("M-s >" . habamax/grep-current-word)
   ("M-s t" . habamax/grep-todo)
   ("C-c t n" . display-line-numbers-mode)
   ("C-c t SPC" . whitespace-mode)
   ("C-c t s" . flyspell-mode)
   ("C-c t m" . flymake-mode)
   ("C-c t l" . hl-line-mode)
   ("C-c t f" . display-fill-column-indicator-mode)
   ("C-c t b" . habamax/toggle-bg)
   ("C-c t v" . visible-mode)
   ("C-x I" . habamax/insert-lorem)
   ("<f9>" . window-toggle-side-windows)
   ("<f10>" . window-toggle-side-windows)
   ("C-c w" . winner-undo)
   ("C-c W" . winner-redo)
   ("C-c <right>" . windmove-right)
   ("C-c <left>" . windmove-left)
   ("C-c <up>" . windmove-up)
   ("C-c <down>" . windmove-down)
   ("C-c S-<right>" . windmove-swap-states-right)
   ("C-c S-<left>" . windmove-swap-states-left)
   ("C-c S-<up>" . windmove-swap-states-up)
   ("C-c S-<down>" . windmove-swap-states-down)
   ("C-=" . text-scale-adjust)
   ("C--" . text-scale-adjust)
   ("C-c SPC SPC" . delete-trailing-whitespace)
   ("C-c SPC u" . revert-buffer)
   ("M-`" . other-frame)
   :repeat-map habamax-other-frame-repeat-map
   ("o" . other-frame)
   :repeat-map habamax-winner-repeat-map
   ("w" . winner-undo)
   ("W" . winner-redo)
   :repeat-map habamax-duplicate-line-repeat-map
   ("d" . habamax/duplicate-line)
   :repeat-map habamax-buffers-like-this-repeat-map
   ("b" . habamax/next-buffer-like-this)
   ("B" . habamax/previous-buffer-like-this)
   :repeat-map habamax-windmove-repeat-map
   ("<right>" . windmove-right)
   ("<left>" . windmove-left)
   ("<up>" . windmove-up)
   ("<down>" . windmove-down)
   ("S-<right>" . windmove-swap-states-right)
   ("S-<left>" . windmove-swap-states-left)
   ("S-<up>" . windmove-swap-states-up)
   ("S-<down>" . windmove-swap-states-down))
  :init
  (when +IS-WINDOWS+
    (require 'habamax-windows))
  (when (or +IS-WINDOWS+ (getenv "WSLENV"))
    (setq epa-pinentry-mode 'loopback))
  (require 'habamax-dev))

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("b" . dired-up-directory))
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (setq ls-lisp-dirs-first t)
  (setq dired-listing-switches "-lAhv --group-directories-first"
        dired-dwim-target t
        dired-hide-details-hide-symlink-targets nil))

(use-package vertico
  :init
  (vertico-mode))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic))
  (setq orderless-matching-styles
        '(orderless-literal orderless-initialism orderless-flex)))

(use-package consult
  :bind (("C-c m" . consult-imenu)
         ("C-c M" . consult-imenu-multi)
         ("C-c C-c f" . consult-focus-lines)
         ("C-c C-c k" . consult-keep-lines)
         ("M-s s" . consult-line)
         ("M-s M-s" . consult-line)
         ("M-g o" . consult-outline)
         ("C-c o a" . consult-org-agenda)
         ([remap goto-line] . consult-goto-line)
         ("M-s g" . consult-grep)
         ("M-s r" . consult-ripgrep)
         ("M-y" . consult-yank-pop))
  :config
  (setq consult-preview-key "M-.")
  (consult-customize
   consult-goto-line consult-line
   consult-focus-lines consult-keep-lines
   :preview-key '(:debounce 0.4 any)))

(use-package paredit
  :bind
  (:map paredit-mode-map
        ("M-s")
        ("C-M-j" . paredit-splice-sexp))
  :hook ((emacs-lisp-mode . paredit-mode)
         (lisp-mode . paredit-mode)
         (sly-mrepl-mode . paredit-mode)))

(use-package wgrep)

(use-package org
  :ensure nil
  :commands (org)
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
         ("C-c o o" . org/open-agenda-file))
  :config
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
  (require 'habamax-org))

(use-package htmlize)

(use-package verb
  :config
  (setq verb-auto-kill-response-buffers t))

(use-package dictionary
  :ensure nil
  :commands dictionary-lookup-definition
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
  :bind (:map markdown-mode-map
              ("M-n")
              ("M-p"))
  :config
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-unordered-list-item-prefix "  - ")
  (setq markdown-asymmetric-header t)
  (setq markdown-command
        "pandoc -s -M fontsize=18pt -M maxwidth=60em --highlight-style tango"))

(use-package erc
  :load-path "lisp"
  :ensure nil
  :commands habamax-erc
  :config
  (require 'habamax-erc))

(use-package erc-hl-nicks
  :after erc
  :config
  (setq erc-hl-nicks-minimum-contrast-ratio 5)
  (add-hook 'wildcharm-hook 'erc-hl-nicks-refresh-colors))

(use-package corfu
  :init
  (global-corfu-mode)
  :config
  (defun corfu-enable-in-minibuffer ()
  "Enable Corfu in the minibuffer if `completion-at-point' is bound."
  (when (where-is-internal #'completion-at-point (list (current-local-map)))
    (setq-local corfu-echo-delay nil
                corfu-popupinfo-delay nil)
    (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
  (when (display-graphic-p)
    (corfu-popupinfo-mode)))

(use-package corfu-terminal
  :if (not (display-graphic-p))
  :after corfu
  :init
  (corfu-terminal-mode t)
  (corfu-echo-mode t))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package tempel
  :bind (("M-*" . tempel-complete)
         ("M-+" . tempel-insert))
  :init
  (defun tempel-setup-capf ()
    (setq completion-at-point-functions
          (cons #'tempel-expand
                completion-at-point-functions)))
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode))

(use-package webpaste
  :commands (webpaste-paste-buffer webpaste-paste-region))

(use-package rainbow-mode
  :commands (rainbow-mode))

(use-package xclip
  :commands xclip-mode)

(use-package ansi-color
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package eglot
  :commands eglot)

(use-package devdocs
  :bind (("C-h D" . devdocs-lookup)))

(use-package gdscript-mode
  :load-path "lisp"
  :bind
  (:map gdscript-mode-map
        ("<f5>" . habamax-gdscript/run-project)
        ("<f6>" . habamax-gdscript/run-current)
        ("C-c r" . habamax-gdscript/run-scene)
        ("C-c C-r" . recompile)
        ("<f7>" . recompile)
        ("C-c n")
        ("<f9>"))
  :config
  (require 'habamax-gdscript)
  :custom
  (gdscript-eglot-version 3))

(use-package sly
  :config
  (setq-default sly-symbol-completion-mode nil))

(use-package zig-mode)

(use-package notmuch
  :load-path "lisp"
  :ensure nil
  :if (executable-find "notmuch")
  :commands (notmuch notmuch-sync)
  :config
  (require 'habamax-notmuch))

(use-package emms
  :load-path "lisp"
  :commands (emms emms-add-directory-tree)
  :bind (("C-c SPC m m" . habamax/emms-play-main)
         ("C-c SPC m 1" . habamax/emms-stream-dnb)
         ("C-c SPC m 2" . habamax/emms-stream-trance)
         ("C-c SPC m 3" . habamax/emms-stream-smooth-jazz)
         ("C-c SPC m 4" . habamax/emms-stream-ambient)
         ("C-c SPC m s" . emms-stop)
         ("C-c SPC m r" . emms-random)
         ("C-c SPC m n" . emms-next))
  :config
  (require 'habamax-emms))

(use-package elfeed
  :config
  (when-let* ((feed-dir (or (getenv "ORG") "~/org"))
              (feeds (concat feed-dir "/.conf/elfeeds.el"))
              (file-exists-p feeds))
    (load feeds)))

;;; init.el ends here
