;;; early-init.el --- emacs early init file -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq gc-cons-threshold most-positive-fixnum)
(add-hook
 'after-init-hook
 (lambda () (setq gc-cons-threshold 800000)))

(setq inhibit-startup-message t
      inhibit-splash-screen t)

(defconst +IS-OSX+ (eq system-type 'darwin))
(defconst +IS-WINDOWS+ (eq system-type 'windows-nt))
(defconst +IS-WSL+ (getenv "WSLENV"))

(setq-default load-prefer-newer t)

(setq-default
 default-frame-alist
 '((font . "Monospace-18")
   (alpha-background . 95)
   (width . 130) (height . 30)
   (fullscreen . maximized)
   (horizontal-scroll-bars . nil)
   (vertical-scroll-bars . nil)
   (menu-bar-lines . 0)
   (tool-bar-lines . 0)))

(add-to-list 'custom-theme-load-path (locate-user-emacs-file "themes/"))
(add-to-list 'custom-theme-load-path "~/prj/wildcharm-theme/")
(ignore-errors (load-theme 'wildcharm t))
;;; early-init.el ends here
