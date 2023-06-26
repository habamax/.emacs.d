;;; habamax.el --- miscelaneous habamax functions -*- lexical-binding: t; -*-
;;; Commentary:
;;  Bunch of misc functions.

;;; Code:

;; to be used with habamax/toggle-bg
(defvar wildcharm-hook nil "After wildcharm-(light-)theme is loaded.")

;;; Visit emacs init file
(defun habamax/open-init-file ()
  (interactive)
  (find-file user-init-file))

(defun habamax/open-secret-file ()
  "Select and open gpg file from org directory."
  (interactive)
  (let ((default-directory (or (getenv "ORG") "~/org")))
    (thread-first
      (lambda (f) (file-relative-name (file-name-sans-extension f)))
      (mapcar (directory-files-recursively default-directory "\\.gpg$"))
      ((lambda (files) (completing-read "Open secret: " files)))
      (concat ".gpg")
      (find-file))))

(defun habamax/open-recent-file ()
  (interactive)
  (find-file
   (completing-read "Open recent: "
                    (mapcar #'abbreviate-file-name
                            (bound-and-true-p recentf-list)))))

;;; Comment a line.
(defun habamax/toggle-comment (arg)
  "Comment or uncomment current line if mark region is not active.
Otherwise call well known `comment-dwim'"
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not mark-active)
           (save-excursion (beginning-of-line) (not (looking-at "\\s-*$"))))
      (progn
        (comment-or-uncomment-region (line-beginning-position)
                                     (line-end-position))
        (forward-line))
    (comment-dwim arg)))

(defun habamax/move-text (arg)
  "Move region or line up/down depending on arg."
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun habamax/move-line-up (arg)
  "Move up region/line."
  (interactive "*p")
  (habamax/move-text (- arg)))

(defun habamax/move-line-down (arg)
  "Move down region/line."
  (interactive "*p")
  (habamax/move-text arg))

;; Next buffer with the same mode
(defun habamax/next-buffer-like-this ()
  "Open next buffer with the same major mode as current."
  (interactive)
  (let ((b-name (buffer-name))
        (b-mode mode-name))
    (next-buffer)
    (while
        (and
         (not (equal mode-name b-mode))
         (not (equal b-name (buffer-name))))
      (next-buffer))))

;; Previous buffer with the same mode
(defun habamax/previous-buffer-like-this ()
  "Open previous buffer with the same major mode as current."
  (interactive)
  (let ((b-name (buffer-name))
        (b-mode mode-name))
    (previous-buffer)
    (while
        (and
         (not (equal mode-name b-mode))
         (not (equal b-name (buffer-name))))
      (previous-buffer))))

(defun habamax/kill-region ()
  "Kill region if mark is active, kill whole line otherwise."
  (interactive)
  (if mark-active
      (kill-region (region-beginning) (region-end))
    (kill-region (line-beginning-position) (line-beginning-position 2))))

(defun habamax/kill-ring-save ()
  "Save region in kill ring if mark is active, save whole line otherwise."
  (interactive)
  (if mark-active
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (line-beginning-position 2))))

(defun habamax/grep-current-word ()
  "Search current word using `grep' and `grep-command'"
  (interactive)
  (grep (concat grep-command (current-word) " .")))

(defun habamax/grep-todo ()
  "Search current TODO:, FIXME: and XXX: using `grep' and `grep-command'"
  (interactive)
  (grep (concat grep-command "\"(TODO|FIXME|XXX):\" .")))

;;;; sort words
(defun habamax/sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.

Prefixed with negative \\[universal-argument], sorts in reverse.

The variable `sort-fold-case' determines whether alphabetic case
affects the sort order.

See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

(defun habamax/insert-lorem ()
  (interactive)
  (let ((path (concat user-emacs-directory "lorem/")))
    (insert-file-contents
     (concat
      path
      (completing-read
       "Insert lorem: "
       (directory-files path
                        nil
                        directory-files-no-dot-files-regexp))))))

(defun habamax/toggle-bg ()
  "Toggle dark/light wildcharm theme."
  (interactive)
  (let ((theme (car custom-enabled-themes)))
    (dolist (loaded-theme custom-enabled-themes)
      (disable-theme loaded-theme))
    (load-theme (if (eq theme 'wildcharm) 'wildcharm-light 'wildcharm) t))
  (run-hooks 'wildcharm-hook))

(defun habamax/toggle-alpha ()
  "Toggle alpha-background (transparency)."
  (interactive)
  (thread-last
    (pcase (frame-parameter nil 'alpha-background)
      (100 92)(_ 100))
    (set-frame-parameter nil 'alpha-background)))

(defun habamax/auth-secret (host)
  "Return secret(password) for specified host from auth-sources."
  (let ((found (nth 0 (auth-source-search :host host :create nil))))
    (when found
      (let ((secret (plist-get found :secret)))
	(if (functionp secret)
	    (funcall secret)
	  secret)))))

(defun habamax/auth-basic (host)
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
