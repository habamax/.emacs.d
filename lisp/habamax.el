;;; habamax.el --- miscelaneous habamax functions -*- lexical-binding: t; -*-
;;; Commentary:
;;  Bunch of misc functions.
;;; Code:
;; todo: make it autoload?

(require 'thingatpt)

(defun habamax-open-user-emacs-file ()
  "Complete and open file from user emacs directory.
Future history is set to init.el."
  (interactive)
  (let* ((project (project-current nil (locate-user-emacs-file "")))
         (root (project-root project))
         (dirs (list root)))
    (project-find-file-in "init.el" dirs project nil)))

(defun habamax-toggle-comment (arg)
  "Comment or uncomment current line if region is not active.
Otherwise call well known `comment-dwim'"
  (interactive "*P")
  (comment-normalize-vars)
  (cond
   ((use-region-p)
    (comment-dwim arg))
   (t
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (forward-line))))

(defun habamax-grep-current-word ()
  "Search current word using `grep' and `grep-command'"
  (interactive)
  (grep (concat grep-command (current-word) " .")))

(defun habamax-grep-todo ()
  "Search current TODO:, FIXME: and XXX: using `grep' and `grep-command'"
  (interactive)
  (grep (concat grep-command "\"(TODO|FIXME|XXX):\" .")))

(defun habamax-toggle-linenr ()
  "Toggle line numbers and hl-line-mode."
  (interactive)
  (setq-local display-line-numbers-type 'relative)
  (display-line-numbers-mode 'toggle)
  (hl-line-mode 'toggle))

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
    (if (frame-parameter nil 'alpha-background) nil 85)
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
