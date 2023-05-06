;;; habamax-org.el  -*- lexical-binding: t; -*-

;;;###autoload
(defun notes ()
  (interactive)
  (find-file (concat org-directory "/notes.org")))

;;;###autoload
(defun todo ()
  (interactive)
  (find-file (concat org-directory "/todo.org")))

;;;###autoload
(defun org/insert-screenshot ()
  (interactive)
  (let* ((img-dir (concat (file-name-sans-extension (buffer-file-name))
                          "_img"))
         (img-name (concat (file-name-sans-extension (buffer-name))
                           "_" (format-time-string "%Y%m%d_%H%M%S") ".png"))
         (filename (concat img-dir "/" img-name)))
    (make-directory img-dir :parents)
    ;; Windows -- use powershell, other(implicit linux) -- use wl-paste
    (shell-command
     (if +IS-WINDOWS+
         (concat "powershell -command \"Add-Type -AssemblyName System.Windows.Forms;"
             "if ($([System.Windows.Forms.Clipboard]::ContainsImage()))"
             "{$image = [System.Windows.Forms.Clipboard]::GetImage();"
             "[System.Drawing.Bitmap]$image.Save('"
             filename
             "',[System.Drawing.Imaging.ImageFormat]::Png);}\"")
       (concat "wl-paste > " filename)))
    (insert (concat "[[file:" filename "]]"))))


(provide 'habamax-org)
