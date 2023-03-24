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
(setq shr-use-fonts nil)

;; ripgrep as grep
(setq grep-command "rg -nS --no-heading ")
(setq grep-use-null-device nil)

;; dired
(setq ls-lisp-dirs-first t)
(setq dired-listing-switches "-al --group-directories-first")
(setq dired-dwim-target t)

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
  :bind ("M-;" . habamax/toggle-comment)
  :bind ("C-c M-;" . habamax/comment-fill-aligned)
  :bind ("C-c d" . habamax/duplicate-line)
  :bind ("M-n" . habamax/move-line-down)
  :bind ("M-p" . habamax/move-line-up)
  :bind ("C-w" . habamax/kill-region)
  :bind ("M-w" . habamax/kill-ring-save)
  :bind ("C-<tab>" . habamax/next-buffer-like-this)
  :bind ("C-`" . habamax/previous-buffer-like-this)
  :bind ("C-c id" . habamax/insert-current-date)
  :bind ("C-c in" . habamax/insert-meeting-notes)
  :bind ([remap list-buffers] . ibuffer)
  :config
  ;; (add-hook 'server-switch-hook 'habamax/focus-frame)
  ;; emacsclient to focus new frame
  (add-hook 'server-after-make-frame-hook 'habamax/focus-frame)

  (defvar habamax-duplicate-line-map
    (let ((map (make-sparse-keymap)))
      (define-key map "d" 'habamax/duplicate-line)
      map)
    "Press d to duplicate line.")
  (put 'habamax/duplicate-line 'repeat-map 'habamax-duplicate-line-map))

(use-package whitespace
  :ensure nil
  :commands (whitespace-mode)
  :config
  (setq whitespace-style '(face trailing tabs spaces tab-mark space-mark))
  (setq whitespace-display-mappings '((tab-mark 9 [8250 9])
                                      (space-mark 32 [183][46])
                                      (space-mark 160 [164][95]))))

(use-package smex :bind (("M-x" . smex)))

(use-package magit
  :commands (magit-status)
  :bind ("C-c g" . magit-status))

(use-package markdown-mode
  :mode "\\.txt$"
  :bind (:map markdown-mode-map
              ("M-n" . nil)
              ("M-p" . nil))
  :config
  (setq markdown-asymmetric-header t)
  (set-face-attribute 'markdown-header-face-1 nil :height 1.4)
  (set-face-attribute 'markdown-header-face-2 nil :height 1.2)
  (set-face-attribute 'markdown-header-face-3 nil :height 1.1))

(use-package erc
  :ensure nil
  :commands erc
  :config
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
        erc-prompt-for-nickserv-password nil)
  (setq erc-timestamp-only-if-changed-flag nil
        erc-timestamp-format "[%H:%M] "
        erc-insert-timestamp-function 'erc-insert-timestamp-left
        erc-fill-column 100))

(use-package erc-hl-nicks :after erc)

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package elfeed
  :commands elfeed
  :config
  (setq elfeed-feeds
        '(("https://www.reddit.com/r/emacs/.rss" reddit emacs)
          ("https://emacsredux.com/atom.xml" emacs)
          ("https://www.opennet.ru/opennews/opennews_all_utf.rss" news)
          ("https://www.linux.org.ru/section-rss.jsp?section=1" news)
          ("https://news.ycombinator.com/rss" news))))


;;; How long it took this time?
(add-hook 'emacs-startup-hook (lambda () (message "%s" (emacs-init-time))))
