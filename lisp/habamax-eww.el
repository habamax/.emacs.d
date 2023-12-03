;;; habamax-eww.el -- helper things for eww -*- lexical-binding: t -*-
;;; Commentary:
;; Taken from prot's configuration
;;; Code:

(defmacro habamax-eww-act-visible-window (&rest body)
  "Run BODY within narrowed-region.
  If region is active run BODY within active region instead.
  Return the value of the last form of BODY."
  `(save-restriction
     (if (use-region-p)
         (narrow-to-region (region-beginning) (region-end))
       (narrow-to-region (window-start) (window-end)))
     ,@body))

(defun habamax-eww--extract-urls (&optional position)
  "Extract links from the current web page.

  Return a list of strings.  Strings are in the form LABEL @ URL.
  When optional argument POSITION is non-nil, include position info
  in the strings too, so strings take the form
  LABEL @ URL ~ POSITION."
  (let (links match)
    (save-excursion
      (goto-char (point-max))
      (while (setq match (text-property-search-backward 'shr-url))
        (let* ((raw-url (prop-match-value match))
               (start-point-prop (prop-match-beginning match))
               (end-point-prop (prop-match-end match))
               (url (when (stringp raw-url)
                      (propertize raw-url 'face 'link)))
               (label (replace-regexp-in-string "\n" " "
                                                (buffer-substring-no-properties
                                                 start-point-prop end-point-prop)))
               (point start-point-prop)
               (line (line-number-at-pos point t))
               (column (save-excursion (goto-char point) (current-column)))
               (coordinates (propertize
                             (format "%d,%d (%d)" line column point)
                             'face 'shadow)))
          (when url
            (if position
                (push (format "%-15s ~ %s  @ %s"
                              coordinates label url)
                      links)
              (push (format "%s  @ %s"
                            label url)
                    links))))))
    links))

(defun habamax-eww-visit-url-on-page (&optional arg)
  "Visit URL from list of links on the page using completion.

  With optional prefix ARG (\\[universal-argument]) open URL in a
  new EWW buffer."
  (interactive "P")
  (when (derived-mode-p 'eww-mode)
    (let* ((links (habamax-eww--extract-urls))
           (selection (completing-read "Browse URL from page: " links nil t))
           (url (replace-regexp-in-string ".*@ " "" selection)))
      (eww url (when arg 4)))))

(defun habamax-eww-jump-to-url-on-page (&optional arg)
  "Jump to URL position on the page using completion.

  When called without ARG (\\[universal-argument]) get URLs only
  from the visible portion of the buffer.  But when ARG is provided
  consider whole buffer."
  (interactive "P")
  (when (derived-mode-p 'eww-mode)
    (let* ((links
            (if arg
                (habamax-eww--extract-urls t)
              (habamax-eww-act-visible-window
               (habamax-eww--extract-urls t))))
           (prompt-scope (if arg
                             (propertize "URL on the page" 'face 'warning)
                           "visible URL"))
           (prompt (format "Jump to %s: " prompt-scope))
           (selection (completing-read prompt links nil t))
           (position (replace-regexp-in-string "^.*(\\([0-9]+\\))[\s\t]+~" "\\1" selection))
           (point (string-to-number position)))
      (goto-char point))))

(provide 'habamax-eww)
;;; habamax-eww.el ends here
