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

(provide 'habamax-windows)
;;; habamax-windows.el ends here
