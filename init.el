;; -*- lexical-binding: t; -*-

;; habamax: personal emacs configuration


(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))


(setq user-full-name "Maxim Kim"
      user-mail-address "habamax@gmail.com")

(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq default-input-method 'russian-computer)

(when +IS-WINDOWS+
  (setq default-process-coding-system '(utf-8-dos . cp1251-dos)))

(setq default-buffer-file-coding-system 'utf-8-unix)

(setq ring-bell-function #'ignore)
(setq scroll-error-top-bottom t)
(setq disabled-command-function nil)
(setq suggest-key-bindings t)
(setq-default fill-column 80)
(setq-default indent-tabs-mode nil)
(setq grep-command "rg -nS --no-heading ")
(setq grep-use-null-device nil)

;; Store all backup and autosave files in the tmp dir
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(electric-pair-mode t)
(delete-selection-mode t)
(save-place-mode 1)
(savehist-mode 1)

(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)
(ido-mode 1)

(winner-mode t)

(setq auto-mode-alist
      (append '(("\\.txt\\'" . rst-mode)) auto-mode-alist))


(if (window-system)
    (cd "~/"))



;; packages
(setq-default
 package-native-compile t
 use-package-always-ensure t
 use-package-enable-imenu-support t)

(require 'use-package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") 'append)

(use-package smex :bind (("M-x" . smex)))

(use-package magit
  :defer
  :commands (magit-status)
  :bind ("C-c g" . magit-status))

(use-package which-key
  :config (which-key-mode))

(add-hook 'emacs-startup-hook #'(lambda () (message "%s" (emacs-init-time))))
