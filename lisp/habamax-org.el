;;; habamax-org.el  -*- lexical-binding: t; -*-

(setq org-directory (or (getenv "ORG") "~/org"))
(setq org-archive-location "archive/%s_archive::")
(setq org-agenda-files
      (append
       '("todo.org" "notes.org" "dates.org" "work.org")
       (mapcar
        (lambda (f) (concat "projects/" f))
        (directory-files (concat org-directory "/projects")
                         nil "^[^.].*\\.org$"))))

(setq org-refile-use-outline-path 'file)
(setq org-refile-targets
      `((,(directory-files-recursively org-directory "\\.org$") :maxlevel . 1)))

(setq org-capture-templates
      '(("t" "Todo" entry (file "todo.org")
         "* TODO %?\n%U" :empty-lines 1 :prepend t)
        ("T" "Todo with link" entry (file "todo.org")
         "* TODO %?\n%U\n\n%i\n%a" :empty-lines 1 :prepend t)
        ("n" "Note" entry (file "notes.org")
         "* %?\n%T\n%i\n" :empty-lines 1 :prepend t)
        ("N" "Note with link" entry (file "notes.org")
         "* %?\n%T\n%i\n%a" :empty-lines 1 :prepend t)
        ("m" "Meeting notes" entry (file "notes.org")
         "* Meeting Notes\n%T\n\nAttendees:\n\n%?\n\nStatus:\n\n"
         :empty-lines 1 :prepend t)))

(setq org-agenda-custom-commands
      '(("k" "Work Agenda"
         ((agenda ""
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'notregexp ":work:"))
                   (org-agenda-start-day "-1d")
                   (org-agenda-span 3)
                   (org-agenda-overriding-header
                    (let* ((caption "── WORK AGENDA ")
                           (width (- (window-width) (length caption))))
                      (format "%s%s\n"
                              caption
                              (make-string width ?─)))
                    )))
          (alltodo "*"
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'notregexp ":work:"))
                   (org-agenda-block-separator nil)
                   (org-agenda-overriding-header
                    (let* ((caption "── WORK TASKS ")
                           (width (- (window-width) (length caption))))
                      (format "\n%s%s\n"
                              caption
                              (make-string width ?─))))))))
        ("n" "Personal Agenda"
         ((agenda ""
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'regexp ":work:"))
                   (org-agenda-start-day "-1d")
                   (org-agenda-span 3)
                   (org-agenda-overriding-header
                    (let* ((caption "── AGENDA ")
                           (width (- (window-width) (length caption))))
                      (format "%s%s\n"
                              caption
                              (make-string width ?─))))))
          (alltodo "*"
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'regexp ":work:"))
                   (org-agenda-block-separator nil)
                   (org-agenda-overriding-header
                    (let* ((caption "── TASKS ")
                           (width (- (window-width) (length caption))))
                      (format "\n%s%s\n"
                              caption
                              (make-string width ?─))))))))))

(setq org-export-with-sub-superscripts '{}
      org-export-headline-levels 5
      org-export-with-email t)
(setq org-html-doctype "html5"
      org-html-html5-fancy t
      org-html-checkbox-type 'html
      org-html-validation-link nil)
(setq org-html-head-include-default-style nil
      org-html-htmlize-output-type 'css)
(setq org-html-style
      (concat "<style type=\"text/css\">\n"
              (with-temp-buffer
                (insert-file-contents
                 (concat user-emacs-directory "org/org.css"))
                (buffer-string))
              "</style>\n"))

(defun org ()
  (interactive)
  (find-file
   (concat org-directory "/"
           (completing-read "Open org file: " org-agenda-files))))

(defun org/insert-screenshot ()
  (interactive)
  (let* ((img-dir (concat
                   (file-name-directory (buffer-file-name))
                   "img-"
                   (file-name-sans-extension (buffer-name))))
         (img-name (concat (file-name-sans-extension (buffer-name))
                           "_" (format-time-string "%Y%m%d_%H%M%S") ".png"))
         (filename (concat img-dir "/" img-name)))
    (make-directory img-dir :parents)
    ;; Windows -- use powershell, other(implicit linux) -- use wl-paste
    (shell-command
     (if +IS-WINDOWS+
         (concat
          "powershell -command \"Add-Type -AssemblyName System.Windows.Forms;"
          "if ($([System.Windows.Forms.Clipboard]::ContainsImage()))"
          "{$image = [System.Windows.Forms.Clipboard]::GetImage();"
          "[System.Drawing.Bitmap]$image.Save('"
          filename
          "',[System.Drawing.Imaging.ImageFormat]::Png);}\"")
       (concat "wl-paste > " filename)))
    (insert (concat "[[file:" filename "]]"))))


(provide 'habamax-org)
