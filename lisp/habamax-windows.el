;;; habamax-windows.el -- windows settings -*- lexical-binding: t -*-

;;; Commentary:
;; Settings related to MS Windows

;;; Code:

(require 'cl-macs)

(set-language-environment 'utf-8)
(setq default-buffer-file-coding-system 'utf-8-unix)
(setq epa-pinentry-mode 'loopback)

(let ((fonts '(("JetBrains Mono NL" . "14")
               ("Dejavu Sans Mono"  . "14")
               ("Consolas"          . "14"))))
  (cl-dolist (fnt fonts)
    (let ((fnt-name (car fnt))
          (fnt-spec (format "%s-%s" (car fnt) (cdr fnt))))
      (when (find-font (font-spec :name fnt-name))
        (add-to-list 'default-frame-alist `(font . ,fnt-spec))
        (set-face-attribute 'fixed-pitch nil :font fnt-spec)
        (set-face-attribute 'fixed-pitch-serif nil :font fnt-spec)
        (cl-return)))))

(provide 'habamax-windows)
;;; habamax-windows.el ends here
