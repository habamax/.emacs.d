(setq-default
 package-native-compile t
 use-package-always-ensure t
 use-package-always-defer t
 use-package-enable-imenu-support t)

(require 'use-package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") 'append)

(use-package smex :bind (("M-x" . smex)))

(use-package magit
  :defer
  :commands (magit-status)
  :bind ("C-c g" . magit-status))

(use-package markdown-mode
  :mode "\\.txt$"
  :config (setq markdown-asymmetric-header t))
