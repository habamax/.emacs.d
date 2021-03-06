(provide 'haba-stuff)

;; Comment a line.
(defun haba/toggle-comment (arg)
  "Comment or uncomment current line if mark region is not active.
Otherwise call well known `comment-dwim'"
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not mark-active) (save-excursion (beginning-of-line) (not (looking-at "\\s-*$"))))
      (progn
        (comment-or-uncomment-region (line-beginning-position) (line-end-position))
        (forward-line))
    (comment-dwim arg)))


(defun haba/comment-fill-aligned (arg)
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

(defun haba/open-line ()
  "Insert newline(s) below the line containing cursor."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(defun haba/join-line ()
  "Join the following line onto the current one.
 (analogous to `C-e', `C-d') or `C-u M-^' or `C-u M-x join-line'.
If the current line is a comment and the pulled-up line is also a comment,
remove the comment characters from that line."
  (interactive)
  (join-line -1)
  ;; If the current line is a comment
  (when (nth 4 (syntax-ppss))
    ;; Remove the comment prefix chars from the pulled-up line if present
    (save-excursion
      ;; Delete all comment-start and space characters
      (while (looking-at (concat "\\s<" ; comment-start char as per syntax table
                                 "\\|" (substring comment-start 0 1) ; first char of `comment-start'
                                 "\\|" "\\s-")) ; extra spaces
        (delete-forward-char 1))
      (insert-char ? ))))


(defun haba/just-one-space ()
  "Replace all whitespace in region with single spaces."
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-region (line-beginning-position) (line-end-position))
      (back-to-indentation)
      (while (re-search-forward "\\s-+" nil t)
        (replace-match " ")))))

(defun haba/duplicate-line (arg)
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

(defun haba/duplicate-line-inc-numbers (num-lines)
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


(defun haba/move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun haba/move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))



(defun haba/move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first. If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))



(defun haba/toggle-window-split ()
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


;; Quick dotemacs/initel opening
(defun haba/open-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file user-init-file))

;; Next buffer with the same mode
(defun haba/next-buffer-like-this ()
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
(defun haba/previous-buffer-like-this ()
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



;; Doesn't work for all modes
(defun haba/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'haba/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))


(defun haba/locate-project-root ()
  "Return path to the project root defined by markers."
  (interactive)
  (setq project-marker-regex
        (mapconcat 'identity
                   '("\\(.git\\)"
                     "\\(.hg\\)"
                     "\\(build.boot\\)")
                   "\\|"))
  (locate-dominating-file
   default-directory
   (lambda (parent)
     (directory-files parent nil project-marker-regex))))


(defun haba/kill-region ()
  "Kill region if mark is active, kill whole line otherwise."
  (interactive)
  (if mark-active
      (kill-region (region-beginning) (region-end))
    (kill-region (line-beginning-position) (line-beginning-position 2))))

(defun haba/kill-ring-save ()
  "Save region in kill ring if mark is active, save whole line otherwise."
  (interactive)
  (if mark-active
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (line-beginning-position 2))))


;;;; Do not include first empty lines in mark-paragraph.
(defun current-line-empty-p ()
  "Return true if current line is empty.

Helper function for advicing `mark-paragraph'."
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

(defun skip-empty-line--mark-paragraph (&rest args)
  "Set cursor on first non-empty line."
  (when (current-line-empty-p)
    (forward-line 1)))

(advice-add 'mark-paragraph :after #'skip-empty-line--mark-paragraph)



;;;; sort words
(defun haba/sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.

Prefixed with negative \\[universal-argument], sorts in reverse.

The variable `sort-fold-case' determines whether alphabetic case
affects the sort order.

See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

;;;; dates
(defun haba/insert-current-date ()
  "Insert current date. Replace ISO date under cursor with current date."
  (interactive)
  (when (string-match "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" (concat "" (thing-at-point 'symbol t)))
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (delete-region (car bounds) (cdr bounds))))
  (insert (format-time-string "%Y-%m-%d"))
  )

;;;; Pretty print XML
(defun haba/pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. 

You need to have `nxml-mode'
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin end)
    (normal-mode))
  (message "Prettified! Did my best!"))

(defun haba/flush-blank-lines (start end)
  "Delete all blank lines in a region."
  (interactive "r")
  (flush-lines "^\\s-*$" start end nil))

(defun haba/collapse-blank-lines (start end)
  "Delete all but one blank lines in a region."
  (interactive "r")
  (replace-regexp "^\n\\{2,\\}" "\n" nil start end))
