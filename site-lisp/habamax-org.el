;;; habamax-org.el  -*- lexical-binding: t; -*-

;;;###autoload
(define-skeleton org/insert-ad-note "Org note admonition" nil
  "#+begin_note\n" _ "\n#+end_note")

;;;###autoload
(define-skeleton org/insert-ad-tip "Org tip admonition" nil
  "#+begin_tip\n" _ "\n#+end_tip")

;;;###autoload
(define-skeleton org/insert-ad-warning "Org warning admonition" nil
  "#+begin_warning\n" _ "\n#+end_warning")

;;;###autoload
(define-skeleton org/insert-ad-caution "Org caution admonition" nil
  "#+begin_caution\n" _ "\n#+end_caution")

;;;###autoload
(define-skeleton org/insert-ad-important "Org important admonition" nil
  "#+begin_important\n" _ "\n#+end_important")

;;;###autoload
(define-skeleton org/insert-src "Org source block" nil
  "#+begin_src" _ "\n#+end_src")

;;;###autoload
(define-skeleton org/insert-example "Org example block" nil
  "#+begin_example\n" _ "\n#+end_example")

;;;###autoload
(define-skeleton org/insert-quote "Org quote block" nil
  "#+begin_quote\n" _ "\n#+end_quote")

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
