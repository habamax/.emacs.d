;;; habamax-gdscript.el -- helper functions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'project)

(defvar habamax-godot-executable "godot")

(defun habamax-gdscript-godot ()
  (interactive)
  (call-process-shell-command
   (concat habamax-godot-executable
           " --path "
           (project-root (project-current))
           " --editor "
           (file-name-sans-extension (buffer-file-name)) ".tscn"
           "&")))

(defun habamax-gdscript--run-scene (scene-file)
  "Run `scene-file' with godot."
  (let ((default-directory (project-root (project-current))))
    (compile (concat habamax-godot-executable " " scene-file))))

(defun habamax-gdscript--strip-scene-file (scene-file)
  "Return relative scene name without .tscn extension."
  (file-relative-name (file-name-sans-extension scene-file)))

(defun habamax-gdscript--find-scene ()
  "Find all scene files in current project."
  (let ((default-directory (project-root (project-current))))
    (mapcar #'habamax-gdscript--strip-scene-file
            (directory-files-recursively default-directory "\\.tscn"))))

(defun habamax-gdscript-run-current ()
  "Run current scene."
  (interactive)
  (habamax-gdscript--run-scene
   (concat (file-name-sans-extension (buffer-file-name)) ".tscn")))

(defun habamax-gdscript-run-project ()
  "Run current godot project."
  (interactive)
  (habamax-gdscript--run-scene ""))

(defun habamax-gdscript-run-scene ()
  "Select and run scene."
  (interactive)
  (let ((default-directory (project-root (project-current))))
    (habamax-gdscript--run-scene
     (concat
      (completing-read "Run scene: " (habamax-gdscript--find-scene))
      ".tscn"))))

(defun habamax-gdscript-run-last ()
  "Run last scene."
  (interactive)
  (cond
   ((string-match "^godot" compile-command) (recompile))
   (t (habamax-gdscript-run-current))))

(defun habamax-gdscript-ts-bool-hl ()
  "Override treesit highlighting for `true' and `false'."
  ;; (interactive)
  (add-to-list 'treesit-font-lock-settings
             (car (treesit-font-lock-rules
                   :language 'gdscript
                   :override t
                   :feature 'boolean
                   '([(false) (true)] @font-lock-constant-face)))
             t))

(provide 'habamax-gdscript)
;;; habamax-gdscript.el ends here
