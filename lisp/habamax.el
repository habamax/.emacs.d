;;; habamax.el  -*- lexical-binding: t; -*-


;;; Comment a line.
;;;###autoload
(defun habamax/toggle-comment (arg)
  "Comment or uncomment current line if mark region is not active.
Otherwise call well known `comment-dwim'"
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not mark-active) (save-excursion (beginning-of-line) (not (looking-at "\\s-*$"))))
      (progn
        (comment-or-uncomment-region (line-beginning-position) (line-end-position))
        (forward-line))
    (comment-dwim arg)))


;;;###autoload
(defun habamax/comment-fill-aligned (arg)
  "Comment out the current line using fill-column to pad and align with comment chars.

For the fill-column set to 80 it should look like:

elisp:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; hello ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

c:
/************************************* hello  *************************************/
"
  (interactive "p")
  (comment-normalize-vars)
  (let* ((comment-style 'aligned)
         (beg (line-beginning-position))
         (end (line-end-position))
         (com-add (/ (- fill-column
                        (- end beg)
                        (string-width comment-start)
                        (* 2 (string-width comment-padding))
                        (string-width comment-end)) 2)))
    (comment-region beg end (+ comment-add com-add))))


;;;###autoload
(defun habamax/duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion
      (end-of-line)
      (setq eol (point))
      (let ((line (buffer-substring bol eol))
            (count arg))
        (while (> count 0)
          (newline)
          (insert line)
          (setq count (1- count))))))
  (next-line arg))


;;;###autoload
(defun habamax/move-line-up ()
  "Move up current line."
  (interactive)
  (let ((column (current-column)))
    (transpose-lines 1)
    (forward-line -2)
    (move-to-column column t)))


;;;###autoload
(defun habamax/move-line-down ()
  "Move down current line."
  (interactive)
  (let ((column (current-column)))
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)
    (move-to-column column t)))


;; Next buffer with the same mode
;;;###autoload
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
;;;###autoload
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


;;;###autoload
(defun habamax/locate-project-root ()
  "Return path to the project root defined by markers."
  (interactive)
  (setq project-marker-regex
        (mapconcat 'identity
                   '("\\(.git\\)"
                     "\\(.hg\\)")
                   "\\|"))
  (locate-dominating-file
   default-directory
   (lambda (parent)
     (directory-files parent nil project-marker-regex))))


;;;###autoload
(defun habamax/kill-region ()
  "Kill region if mark is active, kill whole line otherwise."
  (interactive)
  (if mark-active
      (kill-region (region-beginning) (region-end))
    (kill-region (line-beginning-position) (line-beginning-position 2))))


;;;###autoload
(defun habamax/kill-ring-save ()
  "Save region in kill ring if mark is active, save whole line otherwise."
  (interactive)
  (if mark-active
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (line-beginning-position 2))))


;;;###autoload
(defun habamax/grep-current-word ()
  "Search current word using `grep' and `grep-command'"
  (interactive)
  (grep (concat grep-command (current-word) " .")))


;;;###autoload
(defun habamax/grep-todo ()
  "Search current TODO:, FIXME: and XXX: using `grep' and `grep-command'"
  (interactive)
  (grep (concat grep-command "\\(TODO\\|FIXME\\|XXX\\): .")))


;;;; sort words
;;;###autoload
(defun habamax/sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.

Prefixed with negative \\[universal-argument], sorts in reverse.

The variable `sort-fold-case' determines whether alphabetic case
affects the sort order.

See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))


;;;###autoload
(defun habamax/insert-current-date ()
  "Insert current date. Replace ISO date under cursor with current date."
  (interactive)
  (when (string-match "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" (concat "" (thing-at-point 'symbol t)))
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (delete-region (car bounds) (cdr bounds))))
  (insert (format-time-string "%Y-%m-%d")))


;;;###autoload
(defun habamax/insert-meeting-notes ()
  (interactive)
  (save-excursion
    (insert "# " (format-time-string "%Y-%m-%d") " Meeting Notes\n\n")
    (insert "## Attendees\n\n")
    (insert "## Status\n\n\n\n"))
  (forward-line 3))


(provide 'habamax)
