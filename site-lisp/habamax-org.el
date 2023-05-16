;;; habamax-org.el  -*- lexical-binding: t; -*-

(setq org-directory (or (getenv "DOCS") "~/docs"))
(setq org-agenda-files '("todo.org" "notes.org" "birthdays.org"))

(setq org-refile-use-outline-path 'file
      org-refile-targets '((org-agenda-files :maxlevel . 1)))

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

;; (setq org-agenda-custom-commands
;;       '(("n" "Daily agenda"
;;          ((tags-todo "*"
;;                 ((org-agenda-overriding-header "IMPORTANT TASKS\n")
;;                  (org-agenda-block-separator nil)
;;                  (org-agenda-skip-function '(org-agenda-skip-if nil '(timestamp)))
;;                  (org-agenda-skip-function
;;                   `(org-agenda-skip-entry-if
;;                     'notregexp ,(format "\\[#%s\\]"
;;                                         (char-to-string org-priority-highest))))))
;;           (agenda ""
;;                   ((org-agenda-overriding-header "\nAGENDA\n")
;;                    (org-agenda-block-separator nil)
;;                    (org-agenda-format-date "%A %-e %B %Y")
;;                    (org-agenda-span 1)
;;                    (org-deadline-warning-days 0)
;;                    (org-scheduled-past-days 0)))
;;           (agenda ""
;;                   ((org-agenda-start-on-weekday nil)
;;                    (org-agenda-start-day "+1d")
;;                    (org-agenda-span 3)
;;                    (org-deadline-warning-days 0)
;;                    (org-agenda-block-separator nil)
;;                    (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
;;                    (org-agenda-overriding-header "\nNext three days\n")))
;;           (agenda ""
;;                   ((org-agenda-time-grid nil)
;;                    (org-agenda-start-on-weekday nil)
;;                    (org-agenda-start-day "+4d")
;;                    (org-agenda-span 14)
;;                    (org-agenda-show-all-dates nil)
;;                    (org-deadline-warning-days 0)
;;                    (org-agenda-block-separator nil)
;;                    (org-agenda-entry-types '(:deadline))
;;                    (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
;;                    (org-agenda-overriding-header "\nUpcoming deadlines (+14d)\n")))))))

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
                (insert-file-contents (concat user-emacs-directory "org/org.css"))
                (buffer-string))
              "</style>\n"))

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
