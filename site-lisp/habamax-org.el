;;; habamax-org.el  -*- lexical-binding: t; -*-


(define-skeleton org-ad-note "Org note admonition" nil
  "#+begin_note\n" _ "\n#+end_note")
(define-skeleton org-ad-tip "Org tip admonition" nil
  "#+begin_tip\n" _ "\n#+end_tip")
(define-skeleton org-ad-warning "Org warning admonition" nil
  "#+begin_warning\n" _ "\n#+end_warning")
(define-skeleton org-ad-caution "Org caution admonition" nil
  "#+begin_caution\n" _ "\n#+end_caution")
(define-skeleton org-ad-important "Org important admonition" nil
  "#+begin_important\n" _ "\n#+end_important")
(define-skeleton org-src "Org source block" nil
  "#+begin_src" _ "\n#+end_src")
(define-abbrev org-mode-abbrev-table "bnot" "" 'org-ad-note :system t)
(define-abbrev org-mode-abbrev-table "btip" "" 'org-ad-tip :system t)
(define-abbrev org-mode-abbrev-table "bwar" "" 'org-ad-warn :system t)
(define-abbrev org-mode-abbrev-table "bcau" "" 'org-ad-caution :system t)
(define-abbrev org-mode-abbrev-table "bimp" "" 'org-ad-important :system t)
(define-abbrev org-mode-abbrev-table "bsrc" "" 'org-src :system t)


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
