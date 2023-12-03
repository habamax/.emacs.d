;;; habamax-org.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq org-modules nil)

(setq org-cycle-separator-lines 0)
(setq org-ellipsis " ▶") ; ▶➤◢

(add-hook 'org-mode-hook 'auto-fill-mode)
(setq org-log-done 'time)

(setq org-directory (or (getenv "ORG") "~/org"))
(setq org-archive-location "archive/%s_archive::")
(setq org-agenda-files
      (append
       '("todo.org" "notes.org" "dates.org" "work.org")
       (mapcar
        (lambda (f) (file-name-concat "projects" f))
        (directory-files (file-name-concat org-directory "projects")
                         nil "^[^.].*\\.org$"))))

(setq org-refile-use-outline-path 'file)
(setq org-reverse-note-order t)
(setq org-refile-targets '((org-agenda-files :maxlevel . 1)))

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
         "* Meeting Notes\n%T\n\nAttendees:\n\n%?\n\nDetails:\n\n"
         :empty-lines 1 :prepend t)))

(setq org-agenda-custom-commands
      '(("k" "Work Agenda"
         ((agenda ""
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'notregexp ":work:"))
                   (org-agenda-start-day "-1d")
                   (org-agenda-span 3)
                   (org-agenda-overriding-header "WORK AGENDA\n")))
          (alltodo "*"
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'notregexp ":work:"))
                   (org-agenda-block-separator nil)
                   (org-agenda-overriding-header "\nWORK TASKS\n")))))
        ("n" "Personal Agenda"
         ((agenda ""
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'regexp ":work:"))
                   (org-agenda-start-day "-1d")
                   (org-agenda-span 3)
                   (org-agenda-overriding-header "AGENDA\n")))
          (alltodo "*"
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'regexp ":work:"))
                   (org-agenda-block-separator nil)
                   (org-agenda-overriding-header "\nTASKS\n")))))))

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
                 (locate-user-emacs-file "org/org.css"))
                (buffer-string))
              "</style>\n"))

(org-babel-do-load-languages
 'org-babel-load-languages '((gnuplot . t)))

(defun habamax-org-open-file ()
  (interactive)
  (let* ((pr (project-current nil org-directory))
         (root (project-root pr))
         (dirs (list root)))
    (project-find-file-in "todo.org" dirs pr nil)))

(defun habamax-org-search ()
  (interactive)
  (let ((default-directory org-directory))
    (call-interactively 'grep)))

(defun habamax-org-insert-screenshot ()
  (interactive)
  (let* ((img-dir (concat "img-"
                          (file-name-sans-extension (buffer-name))))
         (img-name (concat (file-name-sans-extension (buffer-name))
                           "-" (format-time-string "%Y%m%d-%H%M%S") ".png"))
         (filename (file-name-concat img-dir img-name)))
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

(defun habamax-org-capture (&optional goto)
  "Call org-capture via completing read. GOTO as in `org-capture'"
  (interactive)
  (let ((label (completing-read
                "Capture: "
                (mapcar (lambda (s) (concat (cadr s)))
                        org-capture-templates))))
    (condition-case error
        (org-capture goto (substring label 0 1))
      ((error quit) (message "Capture aborted")))))

(provide 'habamax-org)
;;; habamax-org.el ends here
