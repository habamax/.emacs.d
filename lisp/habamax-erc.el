;;; habamax-erc.el -- erc settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (not +IS-WINDOWS+) (erc-notifications-mode t))

(setq erc-nick '("habamax" "mxmkm")
      erc-hide-list '("JOIN" "PART" "QUIT")
      erc-join-buffer 'bury
      erc-autojoin-channels-alist '(("Libera.Chat"
                                     "#emacs" "#vim" "#python" "#zig" "#commonlisp"))
      erc-server-reconnect-attempts 5
      erc-server-reconnect-timeout 3)

(setq erc-kill-queries-on-quit t
      erc-kill-server-buffer-on-quit t)

(setq erc-prompt-for-password nil
      erc-prompt-for-nickserv-password nil)

(when-let* ((dir (or (getenv "ORG") "~/org"))
            (file (concat dir "/.conf/erc-ignore.el"))
            (file-exists-p file))
  (load file))

(defun habamax-erc ()
  "Connect to Libera.Chat."
  (interactive)
  (erc :server "irc.libera.chat" :port 6667 :nick "habamax"))

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
  (windmove-left))

(provide 'habamax-erc)
;;; habamax-erc.el ends here
