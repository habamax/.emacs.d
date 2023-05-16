;;; init.el --- emacs init file -*- lexical-binding: t; -*-

;;; Commentary:

;; ¯\_(ツ)_/¯

;;; Code:

;;; How long it took this time?
(add-hook 'emacs-startup-hook (lambda () (message "%s" (emacs-init-time))))

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
    (let ((fonts '(("JetBrains Mono NL" . "14")
                   ("Cascadia Mono SemiLight" . "14")
                   ("Consolas" . "14"))))
      (cl-dolist (fnt fonts)
        (let ((fnt-name (car fnt))
              (fnt-spec (format "%s-%s" (car fnt) (cdr fnt))))
          (when (find-font (font-spec :name fnt-name))
            (add-to-list 'default-frame-alist `(font . ,fnt-spec))
            (set-face-attribute 'fixed-pitch nil :font fnt-spec)
            (set-face-attribute 'fixed-pitch-serif nil :font fnt-spec)
            (cl-return)))))
  (add-to-list 'default-frame-alist '(font . "Monospace-18")))

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
(setq-default line-number-mode t)
(setq describe-bindings-outline t)
(setq set-mark-command-repeat-pop t)
(setq show-paren-when-point-inside-paren t)
(setq tab-always-indent 'complete)

(setq-default abbrev-mode t)
(quietly-read-abbrev-file (concat user-emacs-directory "abbrevs"))

(when +IS-WINDOWS+
  (setq epa-pinentry-mode 'loopback))

;;; Simple HTML renderer to use default font.
(setq shr-use-fonts nil)
(setq shr-color-visible-luminance-min 78)

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
         ("M-`" . other-frame)
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
           (side . bottom)
           (slot . -1))
          ("\\*\\(grep\\|compilation\\|godot - .+\\)\\*"
           (display-buffer-in-side-window)
           (window-height . 0.3)
           (side . bottom)
           (slot . 0))
          ("\\*Customize .*\\*"
           (display-buffer-in-side-window)
           (window-width . 0.3)
           (side . right)
           (slot . -1)))))

(use-package habamax
  :load-path "site-lisp"
  :commands init-file
  :bind (("M-;" . habamax/toggle-comment)
         ("M-o" . delete-blank-lines)
         ("M-n" . habamax/move-line-down)
         ("M-p" . habamax/move-line-up)
         ("C-w" . habamax/kill-region)
         ("M-w" . habamax/kill-ring-save)
         ;;; Use Consult versions instead
         ;; ("C-x C-r" . habamax/recentf-open)
         ;; ("C-c m" . imenu)
         ("C-c b" . habamax/next-buffer-like-this)
         ("C-c B" . habamax/previous-buffer-like-this)
         ("C-c i d" . habamax/insert-current-date)
         ([remap list-buffers] . ibuffer)
         ("C-c d" . habamax/duplicate-line)
         ("M-s g" . habamax/grep-current-word)
         ("M-s t" . habamax/grep-todo)
         ("C-c t n" . display-line-numbers-mode)
         ("C-c t SPC" . whitespace-mode)
         ("C-c t s" . flyspell-mode)
         ("C-c t m" . flymake-mode)
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
  (defun init-file ()
    (interactive)
    (find-file user-init-file)))

(use-package habamax-dev
  :load-path "site-lisp"
  :commands (habamax-dev/run-c-file
             habamax-dev/run-python-file)
  :init
  (add-hook 'c-mode-hook
            (lambda ()
              (local-set-key [f5] 'habamax-dev/run-c-file)
              (c-set-style "linux")
              (setq-local c-basic-offset 4)
              (c-toggle-comment-style -1)))
  (add-hook 'python-mode-hook
            (lambda () (local-set-key [f5] 'habamax-dev/run-python-file))))

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
  :bind (("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x p b" . consult-project-buffer)
         ("C-x r b" . consult-bookmark)
         ("C-x C-r" . consult-recent-file)
         ("C-c m" . consult-imenu)
         ("C-c M" . consult-imenu-multi)
         ("M-g o" . consult-outline)
         ("M-s G" . consult-grep)
         ("M-s r" . consult-ripgrep)
         ("M-y" . consult-yank-pop))
  :config
  (consult-customize
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key "M-."))

(use-package org
  :ensure nil
  :commands (todo notes)
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
         :map org-mode-map
         ;; ("M-g o" . org-goto)
         ("C-c i S" . org/insert-screenshot)
         ("C-c i s" . org/insert-src)
         ("C-c i q" . org/insert-quote)
         ("C-c i e" . org/insert-example)
         ("C-c i c" . org/insert-ad-caution)
         ("C-c i n" . org/insert-ad-note)
         ("C-c i t" . org/insert-ad-tip)
         ("C-c i w" . org/insert-ad-warning)
         ("C-c i i" . org/insert-ad-important)
         :repeat-map habamax-org-map
         ("t" . org-todo))
  :mode (("\\.org$" . org-mode))
  :config
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
  (require 'habamax-org))

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
  (defun erc/layout4 ()
    (interactive)
    (delete-other-windows)
    (split-window-right)
    (split-window-below)
    (windmove-right)
    (split-window-below)
    (windmove-left)
    (switch-to-buffer "#emacs")
    (windmove-right)
    (switch-to-buffer "#vim")
    (windmove-down)
    (switch-to-buffer "#zig")
    (windmove-left)
    (switch-to-buffer "#python")
    (windmove-up))
  (defun erc/layout2 ()
    (interactive)
    (delete-other-windows)
    (split-window-right)
    (switch-to-buffer "#emacs")
    (windmove-right)
    (switch-to-buffer "#vim")
    (windmove-left)))

(use-package erc-hl-nicks
  :after erc
  :config
  (defun erc-nick-hl-override ()
    (erc-hl-nicks-refresh-colors)
    (erc-hl-nicks-force-nick-face "habamax"
                                  (face-background 'font-lock-constant-face)))
  (erc-nick-hl-override)
  (add-hook 'wildcharm-hook 'erc-nick-hl-override))

(use-package webpaste
  :commands (webpaste-paste-buffer webpaste-paste-region))

(use-package rainbow-mode
  :commands (rainbow-mode))

(use-package lorem-ipsum
  :commands (lorem-ipsum-insert-sentences lorem-ipsum-insert-paragraphs))

(use-package xclip
  :init
  (when (and (eq system-type 'gnu/linux) (not (display-graphic-p)))
    (xclip-mode 1)))

(use-package corfu
  :init
  (global-corfu-mode)
  :config
  (corfu-popupinfo-mode))

(use-package corfu-terminal
  :if (not (display-graphic-p))
  :after corfu
  :init
  (corfu-terminal-mode t))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package tempel
  :init
  (defun templ-setup-capf ()
    (setq completion-at-point-functions
          (cons #'tempel-expand
                completion-at-point-functions)))
  (add-hook 'prog-mode-hook 'templ-setup-capf)
  (add-hook 'text-mode-hook 'templ-setup-capf))

(use-package eglot
  :commands eglot)

(use-package gdscript-mode
  :custom
  (gdscript-eglot-version 3))

(use-package sly)

(use-package zig-mode)

(use-package notmuch
  :ensure nil
  :if (executable-find "notmuch")
  :commands (notmuch notmuch-sync)
  :config
  (setq notmuch-show-logo nil
        notmuch-hello-auto-refresh t
        notmuch-hello-recent-searches-max 20
        notmuch-hello-sections '(notmuch-hello-insert-saved-searches
                                 notmuch-hello-insert-alltags)
        notmuch-show-all-tags-list t)
  (setq notmuch-search-oldest-first nil
        notmuch-show-empty-saved-searches t
        notmuch-saved-searches
        `((:name "Unread"
           :query "folder:/Inbox/ and tag:unread"
           :sort-order newest-first
           :key ,(kbd "u"))
          (:name "All"
           :query "folder:/Inbox/ or tag:inbox"
           :sort-order newest-first
           :key ,(kbd "a"))))
  (defun notmuch-sync ()
    (interactive)
    (when (executable-find "mbsync")
      (compile "mbsync -a && notmuch new")))
  (defun notmuch-delete ()
    "It doesn't delete in gmail... Just put emails from Inbox to All..."
    (interactive)
    (let* ((del-tag "deleted")
           (count
            (string-to-number
             (with-temp-buffer
               (shell-command
                (format "notmuch count tag:%s" del-tag) t)
               (buffer-substring-no-properties (point-min) (1- (point-max))))))
           (mail (if (> count 1) "mails" "mail")))
      (unless (> count 0)
        (user-error "No mail marked as `%s'" del-tag))
      (when (yes-or-no-p
             (format "Delete %d %s marked as `%s'?" count mail del-tag))
        (shell-command
         (format "notmuch search --output=files --format=text0 tag:%s | %s"
                 del-tag "xargs -r0 rm"))))))

(use-package emms
  :commands (emms emms-add-directory-tree)
  :config
  (emms-all)
  (setq emms-player-list '(emms-player-mpv emms-player-vlc)
        emms-info-functions '(emms-info-native))
  (setq emms-source-file-default-directory "~/Music/"))

(use-package elfeed
  :config
  (let* ((feed-dir (or (getenv "DOCS") "~/docs"))
         (feeds (concat feed-dir "/emacs/elfeeds.el")))
    (when (file-exists-p feeds)
      (load feeds))))


;;; init.el ends here
