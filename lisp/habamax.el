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

  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))

  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion

      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))

      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )

      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let

  ;; put the point in the lowest line and return
  (next-line arg))

;;;###autoload
(defun habamax/duplicate-line-inc-numbers (num-lines)
  "Duplicate line, preserving cursor column, and increments any numbers found.
  Duplicate a block of optional NUM-LINES lines.  If no optional argument is given,
  then only one line is copied."
  (interactive "p")
  (if (not num-lines) (setq num-lines 0) (setq num-lines (1- num-lines)))
  (let* ((col (current-column))
         (bol (save-excursion (forward-line (- num-lines)) (beginning-of-line) (point)))
         (eol (progn (end-of-line) (point)))
         (line (buffer-substring bol eol)))
    (goto-char bol)
    (while (re-search-forward "[0-9]+" eol 1)
      (let ((num (string-to-number (buffer-substring
                                    (match-beginning 0) (match-end 0)))))
        (replace-match (int-to-string (1+ num))))
      (setq eol (save-excursion (goto-char eol) (end-of-line) (point))))
    (goto-char bol)
    (insert line "\n")
    (move-to-column col)))


;;;###autoload
(defun habamax/move-line-up ()
  "Move up current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))


;;;###autoload
(defun habamax/move-line-down ()
  "Move down current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))


;;;###autoload
(defun habamax/toggle-window-split ()
  "Change vertical and horizontal splits"
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))



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

;;;; dates
;;;###autoload
(defun habamax/insert-current-date ()
  "Insert current date. Replace ISO date under cursor with current date."
  (interactive)
  (when (string-match "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" (concat "" (thing-at-point 'symbol t)))
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (delete-region (car bounds) (cdr bounds))))
  (insert (format-time-string "%Y-%m-%d")))


;;;###autoload
(defun habamax/flush-blank-lines (start end)
  "Delete all blank lines in a region."
  (interactive "r")
  (flush-lines "^\\s-*$" start end nil))


;;;###autoload
(defun habamax/collapse-blank-lines (start end)
  "Delete all but one blank lines in a region."
  (interactive "r")
  (replace-regexp "^\n\\{2,\\}" "\n" nil start end))


;;;###autoload
(defun habamax/other-window (count &optional all-frames)
  "Wrapper around `other-window' to continue to jump to other with key o."
  (interactive "p")
  (other-window count all-frames)
  (message "Use o to jump to next window.")
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "o")
       (lambda () (interactive) (habamax/other-window 1)))
     map)))



(provide 'habamax)
