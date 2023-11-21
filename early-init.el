;;; early-init.el --- emacs early init file -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq gc-cons-threshold most-positive-fixnum)
(add-hook
 'after-init-hook
 (lambda () (setq gc-cons-threshold 10485760)))

(setq inhibit-startup-message t
      inhibit-splash-screen t
      initial-scratch-message nil)

(defconst +IS-OSX+ (eq system-type 'darwin))
(defconst +IS-WINDOWS+ (eq system-type 'windows-nt))
(defconst +IS-WSL+ (getenv "WSLENV"))

(setq-default load-prefer-newer t)

(setq-default
 default-frame-alist
 '((font . "Monospace-18")
   (fullscreen . maximized)
   (horizontal-scroll-bars . nil)
   (vertical-scroll-bars . nil)
   (menu-bar-lines . 0)
   (tool-bar-lines . 0)))

(defconst +THEMES+ '(wildcharm
                     wildcharm-light
                     def-dark
                     def-light))
(setq custom-theme-directory (locate-user-emacs-file "themes/"))
(ignore-errors (load-theme (car +THEMES+) t))
;;; early-init.el ends here
