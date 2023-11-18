;;; def-dark-theme.el --- Almost default dark emacs theme -*- lexical-binding: t; -*-

;; Author: Maxim Kim <habamax@gmail.com>
;; URL: https://github.com/habamax/def-dark-theme
;; Package-Requires: ((emacs "24.1"))
;; Package-Version: 0.7

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Default emacs dark syntax colors, different chrome.

;;; Code:

(deftheme def-dark
  "Almost default dark Emacs theme.")

(let ((fg "#d0d0d0")(bg "#121212")
      (black "#000000")
      (darkgrey "#808080")
      (darkgrey2 "grey20")
      (red "#d75f5f")
      (green "#00af5f")
      (yellow "#d78700")
      (blue "#0087d7")
      (grey "#d0d0d0")
      (white "#f0f0f0")
      (non-text "#585858")
      (match-paren "#ff00af")
      (mode-line-active "#444444")
      (mode-line-inactive "#303030")
      (header-line "#262626")
      (code-block "#1F1F1F"))

  (custom-theme-set-faces
   'def-dark

   ;; standard faces
   `(default
     ((t (:background ,bg :foreground ,fg))))
   `(shadow
     ((t (:foreground ,non-text))))
   `(region
     ((t (:background ,bg :foreground ,blue :inverse-video t))))
   `(trailing-whitespace
     ((t (:foreground ,red :inverse-video t))))
   `(vertical-border
     ((t (:background ,mode-line-inactive :foreground ,non-text))))
   `(mode-line
     ((t (:background ,mode-line-active :foreground ,grey
                              :box (:line-width 1 :color ,non-text)))))
   `(mode-line-inactive
     ((t (:background ,mode-line-inactive :foreground ,darkgrey
                              :box (:line-width 1 :color ,mode-line-active)))))
   `(mode-line-highlight
     ((t (:background ,black
                              :box (:line-width 1 :color ,non-text)))))
   `(mode-line-emphasis
     ((t (:weight bold))))
   `(mode-line-buffer-id
     ((t (:weight bold))))
   `(header-line
     ((t (:background ,header-line :foreground ,fg :extend t
                              :box (:line-width 1 :color ,mode-line-active)))))

   `(cursor
     ((t (:background ,white))))
   `(tab-bar
     ((t (:background ,mode-line-inactive :foreground ,darkgrey))))
   `(tab-bar-tab
     ((t (:background ,mode-line-active :foreground ,white :weight bold
                              :box (:line-width 1 :color ,non-text)))))
   `(tab-bar-tab-inactive
     ((t (:background unspecified :foreground ,grey
                      :box (:line-width 1 :color ,mode-line-active)))))
   `(tab-line
     ((t (:background ,mode-line-inactive :foreground ,darkgrey))))
   `(tab-line-tab
     ((t (:background ,mode-line-active :foreground ,white :weight bold
                              :box (:line-width 1 :color ,non-text)))))
   `(tab-line-tab-current
     ((t (:background ,mode-line-active :foreground ,white :weight bold
                              :box (:line-width 1 :color ,non-text)))))
   `(tab-line-highlight
     ((t (:background ,mode-line-active :foreground ,white :weight bold
                              :box (:line-width 1 :color ,non-text)))))
   `(tab-line-tab-inactive
     ((t (:background unspecified :foreground ,grey
                      :box (:line-width 1 :color ,non-text)))))
   `(tab-line-tab-modified
     ((t (:background unspecified :foreground ,green))))
   `(tab-line-close-highlight
     ((t (:background unspecified :foreground ,red))))

   `(show-paren-match
     ((t :foreground ,match-paren :weight bold)))
   `(show-paren-mismatch
     ((t :background ,match-paren :foreground ,white :weight bold)))

   `(shortdoc-heading
     ((t (:inherit default :weight bold :height 1.3))))
   `(shortdoc-section
     ((t (:inherit default))))

   `(whitespace-space
     ((t (:background unspecified :foreground ,non-text))))
   `(whitespace-line
     ((t nil)))
   `(whitespace-trailing
     ((t (:inherit 'trailing-whitespace))))
   `(whitespace-indentation
     ((t (:inherit 'whitespace-space))))
   `(whitespace-tab
     ((t (:inherit 'whitespace-space))))
   `(whitespace-empty
     ((t (:background ,yellow))))

   `(org-block
     ((t (:background ,code-block :foreground ,fg :extend t))))
   `(org-code
     ((t (:background ,code-block :extend t))))
   `(org-verbatim
     ((t (:background ,code-block :extend t))))
   `(org-date
     ((t (:foreground ,darkgrey))))
   `(org-block-begin-line
     ((t (:foreground ,darkgrey))))
   `(org-block-end-line
     ((t (:inherit org-block-begin-line))))

   `(diff-header
     ((t (:background ,darkgrey2))))
   `(diff-file-header
     ((t (:background ,non-text))))

   `(erc-prompt-face
     ((t (:inverse-video t :weight bold))))
   `(erc-notice-face
     ((t (:foreground ,darkgrey))))
   `(erc-button
     ((t (:inherit link))))
   `(erc-timestamp-face
     ((t (:foreground ,darkgrey :weight unspecified))))
   `(erc-my-nick-face
     ((t (:foreground ,green :weight bold))))
   `(erc-current-nick-face
     ((t (:inherit erc-my-nick-face))))
   `(erc-input-face
     ((t (:foreground ,green))))

   `(gnus-summary-selected
     ((t (:inverse-video t :underline nil))))
   `(gnus-summary-normal-unread
     ((t (:foreground ,white :weight bold))))
   `(gnus-summary-normal-read
     ((t (:foreground ,fg))))
   `(gnus-summary-normal-ticked
     ((t (:foreground ,yellow))))
   `(gnus-summary-normal-ancient
     ((t (:foreground ,darkgrey))))
   `(gnus-summary-cancelled
     ((t (:background unspecified :foreground ,red))))
   `(gnus-header
     ((t (:inherit default))))
   `(gnus-header-name
     ((t (:foreground ,green :weight bold))))
   `(gnus-header-from
     ((t (:foreground ,red :weight bold))))
   `(gnus-header-content
     ((t (:foreground ,fg :weight normal :slant normal))))
   `(gnus-header-subject
     ((t (:foreground ,white :weight bold))))
   `(gnus-header-newsgroups
     ((t (:foreground ,white :weight bold))))))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'def-dark)
;;; def-dark-theme.el ends here
