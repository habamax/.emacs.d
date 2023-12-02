;; init.el --- emacs init file -*- lexical-binding: t; -*-
;;; Commentary:
;; Maxim Kim <habamax@gmail.com>
;;; Code:

(add-hook 'emacs-startup-hook (lambda () (message "%s" (emacs-init-time))))

(setq custom-file (file-name-concat temporary-file-directory "custom-emacs"))

(setq-default buffer-file-coding-system 'utf-8-unix)
(setq default-input-method 'russian-computer)

(setq user-full-name "Maxim Kim"
      user-mail-address "habamax@gmail.com"
      send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-stream-type 'starttls
      smtpmail-smtp-service 587)

(setq use-short-answers t)
(setq ring-bell-function #'ignore)
(setq scroll-error-top-bottom t)
(setq disabled-command-function nil)
(setq-default fill-column 80
              display-fill-column-indicator-character ?╎)
(setq tab-always-indent 'complete)
(setq sentence-end-double-space nil)
(setq duplicate-line-final-position -1)
(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace nil)
(setq search-whitespace-regexp ".*?")
(setq require-final-newline t)
(setq set-mark-command-repeat-pop t)
(setq compilation-scroll-output t)
(setq vc-follow-symlinks t)
(setq bookmark-save-flag 1)
(setq large-file-warning-threshold 20971520)
(setq delete-pair-blink-delay 0)
(setq mouse-autoselect-window t)

(setq tab-bar-show 1
      tab-bar-auto-width-max '(360 30)
      tab-bar-close-button-show nil
      tab-bar-format '(tab-bar-format-history
                       tab-bar-format-tabs
                       tab-bar-separator))

(setq completion-auto-select 'second-tab
      completion-styles '(basic substring partial-completion flex)
      completion-show-help nil
      completion-auto-wrap t
      completions-max-height 12
      completion-cycle-threshold 3)
(setq icomplete-compute-delay 0)
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

(setq font-lock-maximum-decoration 2)

(setq-default abbrev-mode t)
(quietly-read-abbrev-file (locate-user-emacs-file "abbrevs"))

;; Simple HTML renderer to use default font.
(setq shr-use-fonts nil)
(setq shr-color-visible-luminance-min 78)

;; ripgrep as grep and project search
(setq grep-command "rg -nS --no-heading "
      grep-use-null-device nil)
(setq xref-search-program 'ripgrep)

;; Store all backup and autosave files in the tmp dir
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(setq display-buffer-alist
      '(("\\*Calendar*"
         (display-buffer-at-bottom))
        ("\\*.*eshell.*\\*"
         (display-buffer-below-selected)
         (window-height . 12))
        ("\\*Customize.*\\*"
         (display-buffer-use-least-recent-window))))

(setq recentf-max-saved-items 1000)
(recentf-mode)

(repeat-mode)
(electric-pair-mode)
(delete-selection-mode)
(save-place-mode 1)
(savehist-mode 1)
(pixel-scroll-precision-mode)
(fido-mode)

(defalias 'perl-mode 'cperl-mode)
(add-hook 'c-ts-mode-hook
          (lambda ()
            (setq-local c-ts-mode-indent-style 'linux)
            (setq-local c-ts-mode-indent-offset 4)
            (c-ts-mode-toggle-comment-style -1)))
(add-hook 'c-mode-hook
          (lambda ()
            (c-set-style "linux")
            (setq-local c-basic-offset 4)
            (c-toggle-comment-style -1)))

(when +IS-WSL+
  (setq browse-url-firefox-program "firefox.exe")
  (defun browse-url-can-use-xdg-open () nil))
(when (or +IS-WINDOWS+ +IS-WSL+)
  (setq epg-pinentry-mode 'loopback))

(add-to-list 'load-path (locate-user-emacs-file "lisp"))
(require 'habamax)
(require 'habamax-erc)

(global-set-key (kbd "C-=") 'text-scale-adjust)
(global-set-key (kbd "C--") 'text-scale-adjust)
(global-set-key (kbd "M-;") 'habamax-toggle-comment)
(global-set-key [remap list-buffers] 'ibuffer)
(global-set-key [remap eww-search-words] 'habamax-web-search)
(global-set-key (kbd "M-s >") 'habamax-grep-current-word)
(global-set-key (kbd "M-s g") 'habamax-grep)
(global-set-key (kbd "M-s t") 'habamax-grep-todo)
(global-set-key (kbd "C-x I") 'habamax-insert-lorem)

(custom-set-faces
  '(org-document-title ((t (:height 1.5))))
  '(org-agenda-structure ((t (:height 1.5))))
  '(outline-1 ((t (:height 1.5 :weight bold))))
  '(outline-2 ((t (:height 1.3 :weight bold))))
  '(outline-3 ((t (:height 1.1 :weight bold))))
  '(outline-4 ((t (:height 1.0 :weight bold))))
  '(outline-5 ((t (:height 1.0 :weight bold))))
  '(outline-6 ((t (:height 1.0 :weight bold))))
  '(outline-7 ((t (:height 1.0 :weight bold))))
  '(outline-8 ((t (:height 1.0 :weight bold)))))

(when (treesit-available-p)
  (setq treesit-font-lock-level 2)
  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (c-or-c++-mode . c-or-c++-ts-mode)
          (java-mode . java-ts-mode)
          (javascript-mode . js-ts-mode)
          (js-json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode)
          (gdscript-mode . gdscript-ts-mode))))

;; packages
(with-eval-after-load 'package
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") 'append))

(setq package-native-compile t
      native-comp-async-report-warnings-errors nil
      use-package-always-ensure t
      use-package-always-defer t)

(use-package evil
  :bind
  (:map
   evil-normal-state-map
   ("SPC v" . eval-last-sexp)
   ("SPC V" . eval-defun)
   ("SPC b" . switch-to-buffer)
   ("SPC e" . find-file)
   ("SPC f e" . project-find-file)
   ("SPC f p" . project-switch-project)
   ("SPC f i" . habamax-open-user-emacs-file)
   ("SPC f m" . recentf)
   ("SPC f b" . bookmark-jump)
   ("SPC f x" . scratch-buffer)
   ("SPC 8" . habamax-grep-current-word)
   ("SPC z" . imenu)
   ("SPC T SPC" . delete-trailing-whitespace)
   ("SPC t n" . habamax-toggle-linenr)
   ("SPC t s" . flyspell-mode)
   ("SPC t m" . flymake-mode)
   ("SPC t a" . habamax-toggle-alpha)
   ("SPC t t" . habamax-toggle-theme)
   ("SPC t r" . habamax-reload-current-theme)
   ("<backspace>" . dired-jump)
   :map
   minibuffer-mode-map
   ("<escape>" . minibuffer-keyboard-quit)
   :repeat-map habamax-toggle-theme-repeat-map
   ("t" . habamax-toggle-theme))
  :init
  (evil-mode))

(use-package evil-surround
  :after evil
  :init
  (global-evil-surround-mode 1))

(use-package winner-mode
  :ensure nil
  :bind
  (("C-x w w" . winner-undo)
   :repeat-map habamax-winner-repeat-map
   ("w" . winner-undo)
   ("W" . winner-redo))
  :init
  (setq winner-dont-bind-my-keys t)
  (winner-mode))

(use-package dired
  :ensure nil
  :bind (("C-x j" . dired-jump)
         :map dired-mode-map
              ("b" . dired-up-directory))
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (setq ls-lisp-dirs-first t)
  (setq dired-jump-map nil)
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-listing-switches "-lAhv --group-directories-first"
        dired-dwim-target t
        dired-hide-details-hide-symlink-targets nil))

(use-package verb
  :config
  (setq verb-auto-kill-response-buffers t)
  (when (and (treesit-available-p)
             (treesit-language-available-p 'json))
    (setq verb-json-use-mode 'json-ts-mode)))

(use-package whitespace
  :ensure nil
  :hook ((prog-mode text-mode) . whitespace-mode)
  :config
  (setq whitespace-style '(face trailing tabs tab-mark))
  (setq whitespace-display-mappings '((tab-mark 9 [8250 9]))))

(use-package magit
  :bind (("C-x g" . magit-file-dispatch)
         :map magit-mode-map ("C-<tab>")))

(use-package tempel
  :bind (("M-*" . tempel-complete)
         ("M-+" . tempel-insert)
         (:map tempel-map
               ("<tab>" . tempel-next)
               ("TAB" . tempel-next)
               ("<backtab>" . tempel-previous)))
  :hook ((prog-mode text-mode) . tempel-setup-capf)
  :init
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions))))

(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode lisp-mode) . rainbow-delimiters-mode))

(use-package webpaste
  :commands (webpaste-paste-buffer webpaste-paste-region))

(use-package wgrep)

(use-package rainbow-mode
  :commands rainbow-mode)

(use-package ansi-color
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package eglot
  :commands eglot)

(use-package gdscript-mode
  :hook (gdscript-ts-mode . habamax-gdscript-ts-bool-hl)
  :bind
  (:map gdscript-ts-mode-map
        ("C-c <f5>" . habamax-gdscript-godot)
        ("<f5>" . habamax-gdscript-run-project)
        ("<f6>" . habamax-gdscript-run-current)
        ("C-c r" . habamax-gdscript-run-scene)
        ("C-c C-r" . habamax-gdscript-run-last)
        ("<f7>" . recompile)
        ("C-c n") ("<f9>") ([remap mark-defun]))
  :config
  (require 'habamax-gdscript)
  :custom
  (gdscript-eglot-version 3))

(use-package python
  :ensure nil
  :bind (:map python-mode-map
              ("<f5>" . run-py-file)
              :map python-ts-mode-map
              ("<f5>" . run-py-file))
  :config
  (defun run-py-file ()
    "Compile and run single python file"
    (interactive)
    (when-let ((file-name buffer-file-name)
               (python (if (executable-find "python3") "python3" "python")))
      (compile (concat python " " (shell-quote-argument file-name))))))

(use-package markdown-mode
  :config
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-unordered-list-item-prefix "  - ")
  (setq markdown-asymmetric-header t)
  (setq markdown-command
        "pandoc -s -M fontsize=18pt -M maxwidth=60em --highlight-style tango"))

;;; init.el ends here
