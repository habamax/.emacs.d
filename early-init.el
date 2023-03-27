(defconst +IS-OSX+ (eq system-type 'darwin))
(defconst +IS-WINDOWS+ (eq system-type 'windows-nt))

(setq-default
 load-prefer-newer t)
 
(setq-default
 default-frame-alist
 '((fullscreen . maximized)
   (horizontal-scroll-bars . nil)
   (vertical-scroll-bars . nil)
   (menu-bar-lines . 0)
   (tool-bar-lines . 0)))

(setq inhibit-startup-message t
      inhibit-splash-screen t
      initial-scratch-message ";; Welcome, habamax!\n;; Have fun with Emacs!\n\n")

(add-to-list 'custom-theme-load-path (expand-file-name "themes/" user-emacs-directory))
(load-theme 'wildcharm t)
