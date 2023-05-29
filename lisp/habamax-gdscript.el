;;; habamax-gdscript.el -- helper functions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun habamax-gdscript--run-scene (scene-file)
  "Run `scene-file' with godot."
  (let ((default-directory (project-root (project-current))))
    (compile (concat "godot" " " scene-file))))

(defun habamax-gdscript--strip-scene-file (scene-file)
  "Return relative scene name without .tscn extension."
  (file-relative-name (file-name-sans-extension scene-file)))

(defun habamax-gdscript--find-scene ()
  "Find all scene files in current project."
  (let ((default-directory (project-root (project-current))))
    (mapcar #'habamax-gdscript--strip-scene-file
            (directory-files-recursively default-directory "\\.tscn"))))

(defun habamax-gdscript/run-current ()
  "Run current scene."
  (interactive)
  (habamax-gdscript--run-scene
   (concat (file-name-sans-extension (buffer-file-name)) ".tscn")))

(defun habamax-gdscript/run-project ()
  "Run current godot project."
  (interactive)
  (habamax-gdscript--run-scene ""))

(defun habamax-gdscript/run-scene ()
  "Select and run scene."
  (interactive)
  (let ((default-directory (project-root (project-current))))
    (habamax-gdscript--run-scene
     (concat
      (completing-read "Run scene: " (habamax-gdscript--find-scene))
      ".tscn"))))

(provide 'habamax-gdscript)
;;; habamax-gdscript.el ends here
