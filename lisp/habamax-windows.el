;;; habamax-windows.el -- windows settings -*- lexical-binding: t -*-

;;; Commentary:
;; Settings related to MS Windows

;;; Code:

(set-language-environment 'utf-8)
(setq default-buffer-file-coding-system 'utf-8-unix)
(setq epa-pinentry-mode 'loopback)

(when-let* ((fonts '(("JetBrains Mono NL" . "14")
                     ("Dejavu Sans Mono"  . "14")
                     ("Consolas"          . "14")))
            (font (seq-find
                   (lambda (f) (find-font (font-spec :name (car f))))
                   fonts))
            (font-spec (format "%s-%s" (car font) (cdr font))))
  (add-to-list 'default-frame-alist `(font . ,font-spec))
  (set-face-attribute 'fixed-pitch nil :font font-spec)
  (set-face-attribute 'fixed-pitch-serif nil :font font-spec))

(provide 'habamax-windows)
;;; habamax-windows.el ends here
