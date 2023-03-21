;;; -*- lexical-binding: t; -*-
;;; habamax: personal emacs configuration

(setq user-full-name "Maxim Kim"
      user-mail-address "habamax@gmail.com")

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

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

;; ripgrep as grep
(setq grep-command "rg -nS --no-heading ")
(setq grep-use-null-device nil)

;; dired
(setq ls-lisp-dirs-first t)
(setq dired-listing-switches "-al --group-directories-first")

;; Store all backup and autosave files in the tmp dir
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq vc-follow-symlinks t)

(electric-pair-mode t)
(delete-selection-mode t)
(save-place-mode 1)
(savehist-mode 1)
(repeat-mode t)

(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess)
(ido-mode 1)

(winner-mode t)

(if (window-system)
    (cd "~/"))


;;; use packages
(setq-default
 package-native-compile t
 use-package-always-ensure t
 use-package-always-defer t
 use-package-enable-imenu-support t)

(require 'use-package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") 'append)

(use-package habamax
  :load-path "lisp"
  :bind ("M-;" . habamax/toggle-comment)
  :bind ("C-c M-;" . habamax/comment-fill-aligned)
  :bind ("C-c d" . habamax/duplicate-line)
  :bind ("M-n" . habamax/move-line-down)
  :bind ("M-p" . habamax/move-line-up)
  :bind ("C-w" . habamax/kill-region)
  :bind ("M-w" . habamax/kill-ring-save)
  :bind ([remap dabbrev-expand] . hippie-expand)
  :config
  (defvar habamax-duplicate-line-map
    (let ((map (make-sparse-keymap)))
      (define-key map "d" 'habamax/duplicate-line)
      map)
    "Press d to duplicate line.")
  (put 'habamax/duplicate-line 'repeat-map 'habamax-duplicate-line-map))

(use-package smex :bind (("M-x" . smex)))

(use-package magit
  :defer
  :commands (magit-status)
  :bind ("C-c g" . magit-status))

(use-package markdown-mode
  :mode "\\.txt$"
  :config (setq markdown-asymmetric-header t))



;;; How long it took this time?
(add-hook 'emacs-startup-hook #'(lambda () (message "%s" (emacs-init-time))))
