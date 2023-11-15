;;; habamax.el --- miscelaneous habamax functions -*- lexical-binding: t; -*-
;;; Commentary:
;;  Bunch of misc functions.
;;; Code:

(defun habamax-open-user-emacs-file ()
  "Complete and open file from user emacs directory.
Future history is set to init.el."
  (interactive)
  (let* ((project (project-current nil (locate-user-emacs-file "")))
         (root (project-root project))
         (dirs (list root)))
    (project-find-file-in "init.el" dirs project nil)))

(defun habamax-toggle-comment (arg)
  "Comment or uncomment current line if mark region is not active.
Otherwise call well known `comment-dwim'"
  (interactive "*P")
  (comment-normalize-vars)
  (cond
   ((use-region-p)
    (comment-dwim arg))
   (t
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (forward-line))))

(defun habamax-join-line ()
  "Join next line."
  (interactive)
  (delete-indentation 1))

(defun habamax-kill-region ()
  "Kill region if mark is active, kill whole line otherwise."
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end) (use-region-p))
    (kill-region (line-beginning-position) (line-beginning-position 2))))

(defun habamax-kill-ring-save ()
  "Save region in kill ring if mark is active, save whole line otherwise."
  (interactive)
  (if (use-region-p)
      (kill-ring-save (region-beginning) (region-end) (use-region-p))
    (kill-ring-save (line-beginning-position) (line-beginning-position 2))))

(defun habamax-slurp-forward ()
  "Basic implementation of a slurp forward."
  (interactive)
  (save-excursion
    (up-list 1 t t)
    (when (char-equal (char-before) ?\")
      (up-list 1 t t))
    (when-let ((close (char-before))
               (start (point)))
      (delete-char -1)
      (condition-case nil
          (progn
            (when (not
                   (or (char-equal (char-after) 10)
                       (char-equal (char-after) 41)))
              (just-one-space))
            (forward-sexp))
        (error nil))
      (insert close)
      (when (re-search-backward "^\s*)" nil t)
        (delete-indentation))
      (delete-trailing-whitespace start (point))
      (indent-region start (point) nil))))

(defun habamax-diff-current-buffer ()
  "Search current word using `grep' and `grep-command'"
  (interactive)
  (diff-buffer-with-file (buffer-name)))

(defun habamax-grep-current-word ()
  "Search current word using `grep' and `grep-command'"
  (interactive)
  (grep (concat grep-command (current-word) " .")))

(defun habamax-grep-todo ()
  "Search current TODO:, FIXME: and XXX: using `grep' and `grep-command'"
  (interactive)
  (grep (concat grep-command "\"(TODO|FIXME|XXX):\" .")))

(defun habamax-insert-lorem ()
  "Select and insert text file from lorem/ directory located in
`user-emacs-directory'"
  (interactive)
  (let ((path (locate-user-emacs-file "lorem/")))
    (insert-file-contents
     (file-name-concat
      path
      (completing-read
       "Insert lorem: "
       (directory-files path
                        nil
                        directory-files-no-dot-files-regexp))))))

(defun habamax-toggle-theme ()
  "Toggle my themes."
  (interactive)
  (let* ((theme (car custom-enabled-themes))
         (idx (or (seq-position +THEMES+ theme) -1))
         (next-theme (or (seq-elt +THEMES+ (1+ idx))
                         (car +THEMES+))))
    (mapc #'disable-theme custom-enabled-themes)
    (ignore-errors (load-theme next-theme t))))

(defun habamax-toggle-alpha ()
  "Toggle alpha-background (transparency)."
  (interactive)
  (thread-last
    (pcase (frame-parameter nil 'alpha-background)
      (90 nil)(_ 90))
    (set-frame-parameter nil 'alpha-background)))

(defun habamax-reload-current-theme ()
  "Reload current theme."
  (interactive)
  (let ((theme (car custom-enabled-themes)))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t)))

(defun habamax-open-file-manager ()
  "Open file-manager for the current file."
  (interactive)
  (let ((filename (or (buffer-file-name)
                      (dired-get-file-for-visit))))
    (cond
     ((and +IS-WINDOWS+ filename)
      (call-process-shell-command
       (format "explorer.exe /select, \"%s\""
               (string-replace "/" "\\" filename))))
     ((and +IS-WSL+ filename)
      (call-process-shell-command
       (format "explorer.exe /select, \"%s\""
               (if (string-match "/mnt/[c-z]/.*" filename)
                   (string-trim (shell-command-to-string (concat "wslpath -w '" filename "'")))
                 (string-replace "/" "\\" filename)))))
     (filename
      (call-process-shell-command
       (format "nautilus --select \"%s\" &" filename))))))

(defun habamax-web-search ()
  "Search the web for the text in the region.
If region is active, search the web for the text between region
beginning and end. Else, prompt the user for a search string."
  (interactive)
  (let ((query (if (use-region-p)
                   (buffer-substring (region-beginning) (region-end))
                 (read-string "Search web for: "
                              nil
                              'web-search-history
                              (word-at-point t)))))
    (browse-url (format "https://google.com/search?q=%s"
                        (url-hexify-string query)))))

(defun habamax-auth-secret (host)
  "Return secret(password) for specified host from auth-sources."
  (let ((found (nth 0 (auth-source-search :host host :create nil))))
    (when found
      (let ((secret (plist-get found :secret)))
        (if (functionp secret)
            (funcall secret)
          secret)))))

(defun habamax-auth-basic (host)
  "Return base64 encoded login:password for specified host from auth-sources."
  (let ((found (nth 0 (auth-source-search :host host :create nil))))
    (when found
      (let ((secret (plist-get found :secret))
            (user (plist-get found :user)))
        (base64url-encode-string
         (format "%s:%s"
                 user
                 (if (functionp secret)
                     (funcall secret)
                   secret)))))))

(provide 'habamax)
;;; habamax.el ends here
