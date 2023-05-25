;;; habamax-gdscript.el -- helper functions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun habamax-gdscript-run-current ()
  (interactive)
  (let ((default-directory (project-root (project-current))))
    (compile (concat
              "godot"
              " "
              (file-name-sans-extension (buffer-file-name))
              ".tscn"))))

(defun habamax-gdscript-run-project ()
  (interactive)
  (let ((default-directory (project-root (project-current))))
    (compile "godot")))

(provide 'habamax-gdscript)
;;; habamax-gdscript.el ends here
