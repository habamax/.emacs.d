;;; habamax-windows.el -- windows settings -*- lexical-binding: t -*-
;;; Commentary:
;; Settings related to MS Windows

;;; Code:

(when-let* ((fonts '(("Iosevka Fixed SS04" . "17")
                     ("JetBrains Mono NL" . "14")
                     ("Consolas"          . "14")))
            (font (seq-find
                   (lambda (f) (find-font (font-spec :name (car f))))
                   fonts))
            (name (format "%s-%s" (car font) (cdr font))))
  (add-to-list 'default-frame-alist `(font . ,name))
  (set-face-attribute 'fixed-pitch nil :font name)
  (set-face-attribute 'fixed-pitch-serif nil :font name))

(when (member "Segoe UI Emoji" (font-family-list))
  (set-fontset-font
    t 'emoji (font-spec :family "Segoe UI Emoji") nil 'prepend))

;; (when (member "Noto Color Emoji" (font-family-list))
;;   (set-fontset-font
;;     t 'emoji (font-spec :family "Noto Color Emoji") nil 'prepend))


(provide 'habamax-windows)
;;; habamax-windows.el ends here
