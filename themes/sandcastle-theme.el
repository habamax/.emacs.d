;;; sandcastle-theme.el --- Light mid(low) contrast Emacs theme -*- lexical-binding: t; -*-

;; Author: Maxim Kim <habamax@gmail.com>
;; URL: https://github.com/habamax/sandcastle-theme
;; Package-Requires: ((emacs "24.1"))
;; Package-Version: 0.1

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

;; Light mid(low) contrast Emacs theme.

;;; Code:

(deftheme sandcastle
  "Light mid(low) contrast Emacs theme.")

(let* ((classTTY '((type tty)))
       (fg "#000000")(bg "#d7d6d0")
       (black "#000000")(darkgrey "#707070")
       (red "#af0000")(bright-red "#d70000")
       (green "#2f7f00")(bright-green "#3f8f0f")
       (yellow "#af5f00")(bright-yellow "#d78700")
       (blue "#004fa7")(bright-blue "#0078c7")
       (magenta "#870087")(bright-magenta "#9f009f")
       (cyan "#005f5f")(bright-cyan "#007f7f")
       (grey "#d0d0d0")(white "#ffffff")
       (purple "#4f00c7")(comment "#7f6f4f")
       (yellow1 "#875f00")(yellow2 "#af5f00")
       (grey1 "#cac9c3")(grey2 "#dfded9")(grey3 "#e7e6e0")
       (non-text "#979690")
       (match-paren "#ff00af")(match "#e0c7e0")
       (mode-line-active "#bebaa9")(mode-line-inactive "#c0bfb9")
       (menu "#efeee9")
       (header-line "#cbcac4")
       (hl-line "#c9c8c2")
       (diff-added-bg "#b7d7af")(diff-refine-added-bg "#e3fcd7")
       (diff-added-fg "#005f00")
       (diff-removed-bg "#d7b7af")(diff-refine-removed-bg "#fce0d7")
       (diff-removed-fg "#5f0000")
       (diff-changed-bg "#d7d5af")(diff-refine-changed-bg "#fffcd7")
       (diff-changed-fg "#5f5f00")
       (diff-ancestor-bg "#afafd7")(diff-refine-ancestor-bg "#d7d7ff")
       (diff-ancestor-fg "#00005f")
       (outline-1 black)
       (outline-2 "#5f005f")
       (outline-3 "#000087")
       (outline-4 "#875f5f")
       (outline-5 "#005f5f")
       (outline-6 "#af875f")
       (outline-7 "#005f87")
       (outline-8 darkgrey)
       (code-block "#e0dfd9"))

  (custom-theme-set-faces
   'sandcastle

   ;; standard faces
   `(default
     ((t (:background ,bg :foreground ,fg))))
   `(shadow
     ((t (:foreground ,non-text))))
   `(link
     ((t (:foreground ,blue :underline t))))
   `(link-visited
     ((t (:foreground ,magenta :underline t))))
   `(highlight
     ((t (:background ,white :foreground ,blue :inverse-video t))))
   `(region
     ((t (:background ,white :foreground ,bright-blue :inverse-video t))))
   `(secondary-selection
     ((t (:background ,white :foreground ,bright-cyan :inverse-video t))))
   `(trailing-whitespace
     ((t (:foreground ,bright-red :inverse-video t))))
   `(line-number
     ((t (:inherit default :foreground ,non-text))))
   `(line-number-current-line
     ((t (:inherit default :foreground unspecified :weight bold))))
   `(line-number-major-tick
     ((t (:inherit default :foreground ,darkgrey :weight bold))))
   `(line-number-minor-tick
     ((t (:inherit default :foreground ,darkgrey))))
   `(escape-glyph
     ((t (:foreground ,red))))
   `(homoglyph
     ((t (:inherit 'escape-glyph))))
   `(nobreak-space
     ((t (:inherit 'escape-glyph :underline t))))
   `(nobreak-hyphen
     ((t (:inherit 'escape-glyph))))
   `(mode-line
     ((t (:background ,mode-line-active :foreground ,fg
                      :box (:line-width 1 :color ,non-text)))))
   `(mode-line-inactive
     ((t (:background ,mode-line-inactive :foreground ,darkgrey
                      :box (:line-width 1 :color ,mode-line-active)))))
   `(mode-line-highlight
     ((t (:background ,bg
                      :box (:line-width 1 :color ,non-text)))))
   `(mode-line-emphasis
     ((t (:weight bold))))
   `(mode-line-buffer-id
     ((t (:weight bold))))
   `(header-line
     ((t (:background ,header-line :foreground ,fg :extend t
                      :box (:line-width 1 :color ,darkgrey)))))
   `(vertical-border
     ((,classTTY (:background ,mode-line-inactive :foreground ,mode-line-inactive))
      (t (:background ,non-text :foreground ,non-text))))
   `(window-divider
     ((t (:foreground ,mode-line-inactive))))
   `(window-divider-first-pixel
     ((t (:foreground ,mode-line-active))))
   `(window-divider-last-pixel
     ((t (:foreground ,mode-line-active))))
   `(minibuffer-prompt
     ((t (:foreground unspecified :weight bold))))
   `(fringe
     ((t (:foreground ,non-text :background unspecified))))
   `(separator-line
     ((t (:foreground ,non-text :underline t))))
   ;; -scroll-bar
   `(cursor
     ((t (:background ,fg))))
   ;; -tool-bar
   `(tab-bar
     ((t (:background ,mode-line-inactive :foreground ,darkgrey))))
   `(tab-bar-tab
     ((t (:background ,mode-line-active :foreground ,black :weight bold
                      :box (:line-width 1 :color ,non-text)))))
   `(tab-bar-tab-inactive
     ((t (:background unspecified :foreground ,fg
                      :box (:line-width 1 :color ,non-text)))))
   `(tab-line
     ((t (:background ,mode-line-inactive :foreground ,darkgrey))))
   `(tab-line-tab
     ((t (:background ,mode-line-active :foreground ,black :weight bold
                      :box (:line-width 1 :color ,non-text)))))
   `(tab-line-tab-current
     ((t (:background ,mode-line-active :foreground ,black :weight bold
                      :box (:line-width 1 :color ,non-text)))))
   `(tab-line-highlight
     ((t (:background ,mode-line-active :foreground ,black :weight bold
                      :box (:line-width 1 :color ,non-text)))))
   `(tab-line-tab-inactive
     ((t (:background unspecified :foreground ,fg
                      :box (:line-width 1 :color ,non-text)))))
   `(tab-line-tab-modified
     ((t (:background unspecified :foreground ,green))))
   `(tab-line-close-highlight
     ((t (:background unspecified :foreground ,red))))
   `(help-key-binding
     ((t (:background unspecified :foreground unspecified :weight bold))))
   `(error
     ((t (:foreground ,bright-red :weight bold))))
   `(warning
     ((t (:foreground ,yellow :weight bold))))
   `(success
     ((t (:foreground ,green :weight bold))))
   `(menu
     ((t (:background ,mode-line-inactive :foreground ,fg))))
   `(tty-menu-enabled-face
     ((t (:background ,menu :foreground ,fg))))
   `(tty-menu-disabled-face
     ((t (:background ,menu :foreground ,darkgrey))))
   `(tty-menu-selected-face
     ((t (:background ,mode-line-active :foreground ,fg :weight bold))))

   `(show-paren-match
     ((t :foreground ,match-paren :weight bold)))
   `(show-paren-mismatch
     ((t :background ,match-paren :foreground ,black :weight bold)))

   ;; ansi colors
   `(ansi-color-black
     ((t (:background ,black :foreground ,black))))
   `(ansi-color-bright-black
     ((t (:background ,darkgrey :foreground ,darkgrey))))
   `(ansi-color-red
     ((t (:background ,red :foreground ,red))))
   `(ansi-color-bright-red
     ((t (:background ,bright-red :foreground ,bright-red))))
   `(ansi-color-green
     ((t (:background ,green :foreground ,green))))
   `(ansi-color-bright-green
     ((t (:background ,bright-green :foreground ,bright-green))))
   `(ansi-color-yellow
     ((t (:background ,yellow :foreground ,yellow))))
   `(ansi-color-bright-yellow
     ((t (:background ,bright-yellow :foreground ,bright-yellow))))
   `(ansi-color-blue
     ((t (:background ,blue :foreground ,blue))))
   `(ansi-color-bright-blue
     ((t (:background ,bright-blue :foreground ,bright-blue))))
   `(ansi-color-magenta
     ((t (:background ,magenta :foreground ,magenta))))
   `(ansi-color-bright-magenta
     ((t (:background ,bright-magenta :foreground ,bright-magenta))))
   `(ansi-color-cyan
     ((t (:background ,cyan :foreground ,cyan))))
   `(ansi-color-bright-cyan
     ((t (:background ,bright-cyan :foreground ,bright-cyan))))
   `(ansi-color-white
     ((t (:background ,grey :foreground ,grey))))
   `(ansi-color-bright-white
     ((t (:background ,white :foreground ,white))))

   ;; font-lock
   `(font-lock-string-face
     ((t (:foreground ,green))))
   `(font-lock-comment-face
     ((t (:foreground ,comment))))
   `(font-lock-keyword-face
     ((t (:foreground ,blue))))
   `(font-lock-preprocessor-face
     ((t (:foreground ,purple))))
   `(font-lock-builtin-face
     ((t (:foreground ,magenta))))
   `(font-lock-type-face
     ((t (:foreground ,yellow))))
   `(font-lock-function-name-face
     ((t (:foreground ,bright-magenta))))
   `(font-lock-variable-name-face
     ((t (:foreground ,bright-cyan))))
   `(font-lock-constant-face
     ((t (:foreground ,red))))
   `(font-lock-warning-face
     ((t (:foreground ,bright-yellow :weight bold))))

   `(font-lock-number-face
     ((t (:foreground ,red))))
   `(font-lock-escape-face
     ((t (:foreground ,yellow))))
   `(font-lock-function-call-face
     ((t (:foreground ,magenta))))
   `(font-lock-regexp-face
     ((t (:foreground ,bright-green))))
   `(font-lock-delimiter-face
     ((t (:foreground ,magenta))))
   `(font-lock-property-use-face
     ((t (:foreground ,cyan))))
   `(font-lock-bracket-face
     ((t (:foreground ,purple))))

   `(elisp-shorthand-font-lock-face
     ((t (:foreground ,bright-cyan :weight bold))))

   ;; isearch
   `(isearch
     ((t (:background ,white :foreground ,bright-yellow :inverse-video t))))
   `(isearch-group-1
     ((t (:background ,white :foreground ,yellow2 :inverse-video t))))
   `(isearch-group-2
     ((t (:background ,white :foreground ,yellow1 :inverse-video t))))
   `(lazy-highlight
     ((t (:background ,white :foreground ,green :inverse-video t))))
   `(isearch-fail
     ((t (:background ,diff-removed-bg :foreground ,diff-removed-fg))))

   ;; replace.el
   `(match
     ((t (:background ,match))))

   ;; global-hl-line-mode
   `(hl-line
     ((t (:background ,hl-line :foreground unspecified))))

   ;; widget
   `(custom-button
     ((t (:background ,grey1 :foreground ,fg :extend t
                      :box (:line-width (2 . 2) :style released-button)))))
   `(custom-button-pressed
     ((t (:background ,grey1 :foreground ,fg :extend t
                      :box (:line-width (2 . 2) :style pressed-button)))))
   `(custom-button-mouse
     ((t (:background ,hl-line :foreground ,fg :extend t
                      :box (:line-width (2 . 2) :style released-button)))))
   `(custom-state
     ((t (:foreground ,green))))
   `(custom-group-tag
     ((t (:foreground ,bright-magenta :weight bold))))
   `(widget-field
     ((t (:background ,grey1 :foreground ,fg :extend t))))
   `(widget-inactive
     ((t (:foreground ,darkgrey))))
   `(widget-button-pressed
     ((t (:foreground ,bright-red))))
   `(widget-documentation
     ((t (:foreground ,green))))

   ;; customize
   `(custom-variable-tag
     ((t (:weight bold))))

   ;; shortdoc
   `(shortdoc-heading
     ((t (:inherit default :weight bold :height 1.3))))
   `(shortdoc-section
     ((t (:inherit default))))

   ;; package
   `(package-help-section-name
     ((t (:foreground unspecified :weight bold))))
   `(package-status-installed
     ((t (:foreground ,darkgrey))))

   ;; dired
   `(dired-header
     ((t (:foreground ,blue :weight bold))))
   `(dired-directory
     ((t (:foreground unspecified :weight bold))))
   `(dired-symlink
     ((t (:foreground ,cyan :weight bold :inherit nil))))
   `(dired-broken-symlink
     ((t (:background ,bright-red :foreground ,white))))
   `(dired-special
     ((t (:foreground ,magenta :inherit nil))))
   `(dired-perm-write
     ((t (:foreground ,yellow :inherit nil))))

   ;; completion
   `(icomplete-first-match
     ((t (:foreground ,green :weight bold))))
   `(icomplete-selected-match
     ((t (:background ,hl-line))))
   `(completions-common-part
     ((t (:foreground ,red :weight bold))))
   `(completions-first-difference
     ((t (:foreground ,blue :weight bold))))
   `(completions-annotations
     ((t (:foreground ,darkgrey))))

   ;; ido
   `(ido-first-match
     ((t (:foreground ,green :weight bold))))
   `(ido-only-match
     ((t (:inherit 'ido-first-match))))
   `(ido-virtual
     ((t (:foreground ,darkgrey))))
   `(ido-subdir
     ((t (:foreground ,fg :weight bold))))
   ;; check how good it is
   `(ido-indicator
     ((t (:background ,bright-red :foreground ,yellow))))

   ;; compilation
   `(compilation-mode-line-fail
     ((t (:foreground ,red :weight bold))))
   `(compilation-mode-line-exit
     ((t (:foreground ,green :weight bold))))
   `(compilation-line-number
     ((t (:foreground ,darkgrey))))

   ;; whitespace
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

   ;; message
   `(message-header-name
     ((t (:foreground ,magenta))))
   `(message-header-subject
     ((t (:foreground ,black :weight bold))))
   `(message-header-to
     ((t (:foreground ,black))))
   `(message-header-other
     ((t (:foreground ,fg))))
   `(message-header-cc
     ((t (:foreground ,darkgrey))))
   `(message-separator
     ((t (:background ,grey1 :foreground ,fg :extend t))))
   `(message-mml
     ((t (:foreground ,comment))))

   ;; bookmark
   `(bookmark-face
     ((t (:background unspecified :foreground ,yellow))))

   ;; info
   `(info-title-1
     ((t (:foreground ,black :weight bold))))
   `(info-title-2
     ((t (:foreground ,black :weight bold))))
   `(info-title-3
     ((t (:foreground ,black :weight bold))))
   `(info-title-4
     ((t (:foreground ,black :weight bold))))
   `(info-menu-header
     ((t (:inherit info-title-3))))
   `(info-node
     ((t (:foreground ,yellow :weight bold))))

   ;; edmacro
   `(edmacro-label
     ((t (:foreground ,blue :weight bold))))

   ;; outline
   `(outline-1
     ((t (:foreground ,outline-1 :weight bold))))
   `(outline-2
     ((t (:foreground ,outline-2 :weight bold))))
   `(outline-3
     ((t (:foreground ,outline-3 :weight bold))))
   `(outline-4
     ((t (:foreground ,outline-4 :weight bold))))
   `(outline-5
     ((t (:foreground ,outline-5 :weight bold))))
   `(outline-6
     ((t (:foreground ,outline-6 :weight bold))))
   `(outline-7
     ((t (:foreground ,outline-7 :weight bold))))
   `(outline-8
     ((t (:foreground ,outline-8 :weight bold))))

   ;; org
   `(org-meta-line
     ((t (:foreground ,darkgrey))))
   `(org-document-info-keyword
     ((t (:inherit org-meta-line))))
   `(org-special-keyword
     ((t (:inherit org-meta-line))))
   `(org-block
     ((t (:background ,code-block :foreground ,fg :extend t))))
   `(org-block-begin-line
     ((t (:background ,code-block :foreground ,darkgrey :extend t))))
   `(org-block-end-line
     ((t (:background ,code-block :foreground ,darkgrey :extend t))))
   `(org-document-title
     ((t (:foreground ,black :weight bold))))
   `(org-document-info
     ((t (:foreground ,fg))))
   `(org-drawer
     ((t (:foreground ,purple))))
   `(org-code
     ((t (:background ,code-block :foreground ,cyan :extend t))))
   `(org-verbatim
     ((t (:background ,code-block :foreground ,magenta :extend t))))
   `(org-footnote
     ((t (:foreground ,darkgrey))))
   `(org-ellipsis
     ((t (:foreground ,yellow))))
   `(org-formula
     ((t (:foreground ,red))))
   `(org-latex-and-related
     ((t (:foreground ,yellow))))
   `(org-tag
     ((t (:foreground ,darkgrey :weight normal))))
   `(org-level-1
     ((t (:inherit outline-1))))
   `(org-level-2
     ((t (:inherit outline-2))))
   `(org-level-3
     ((t (:inherit outline-3))))
   `(org-level-4
     ((t (:inherit outline-4))))
   `(org-level-5
     ((t (:inherit outline-5))))
   `(org-level-6
     ((t (:inherit outline-6))))
   `(org-level-7
     ((t (:inherit outline-7))))
   `(org-level-8
     ((t (:inherit outline-8))))
   `(org-todo
     ((t (:foreground ,bright-red :weight bold))))
   `(org-done
     ((t (:foreground ,bright-green :weight bold))))
   `(org-date
     ((t (:foreground ,darkgrey))))
   `(org-sexp-date
     ((t (:foreground ,bright-cyan))))
   `(org-headline-done
     ((t (:foreground unspecified))))
   `(org-checkbox
     ((t (:foreground ,darkgrey :weight normal))))
   `(org-dispatcher-highlight
     ((t (:foreground ,bright-red :weight bold))))
   `(org-agenda-structure
     ((t (:foreground ,black :weight bold))))
   `(org-agenda-structure-filter
     ((t (:foreground ,bright-red :weight bold))))
   `(org-date-selected
     ((t (:background ,bright-magenta :foreground ,bg))))
   `(org-agenda-date
     ((t (:foreground ,blue :weight normal))))
   `(org-agenda-date-today
     ((t (:foreground ,bright-magenta :weight bold))))
   `(org-agenda-current-time
     ((t (:foreground ,bright-magenta))))
   `(org-agenda-done
     ((t (:foreground ,green))))
   `(org-scheduled-today
     ((t (:foreground ,bright-cyan))))
   `(org-scheduled
     ((t (:foreground ,cyan))))
   `(org-scheduled-previously
     ((t (:foreground ,red))))
   `(org-upcoming-deadline
     ((t (:foreground ,bright-yellow))))
   `(org-imminent-deadline
     ((t (:foreground ,bright-red :weight bold))))
   `(org-time-grid
     ((t (:foreground ,darkgrey :weight normal))))
   `(org-table
     ((t (:foreground ,fg))))
   `(org-mode-line-clock-overrun
     ((t (:background ,yellow :foreground ,white))))

   ;; gnus
   `(gnus-button
     ((t (:inherit link))))
   `(gnus-group-mail-1
     ((t (:foreground ,green :weight bold))))
   `(gnus-group-mail-1-empty
     ((t (:foreground ,green))))
   `(gnus-group-mail-2
     ((t (:foreground ,yellow :weight bold))))
   `(gnus-group-mail-2-empty
     ((t (:foreground ,yellow))))
   `(gnus-group-mail-3
     ((t (:foreground ,fg :weight bold))))
   `(gnus-group-mail-3-empty
     ((t (:foreground ,fg))))
   `(gnus-group-mail-4
     ((t (:foreground ,darkgrey :weight bold))))
   `(gnus-group-mail-4-empty
     ((t (:foreground ,darkgrey))))
   `(gnus-group-mail-5
     ((t (:foreground ,darkgrey :weight bold))))
   `(gnus-group-mail-5-empty
     ((t (:foreground ,darkgrey))))
   `(gnus-group-mail-6
     ((t (:foreground ,darkgrey :weight bold))))
   `(gnus-group-mail-6-empty
     ((t (:foreground ,darkgrey))))
   `(gnus-group-mail-low
     ((t (:foreground ,darkgrey :weight bold))))
   `(gnus-group-mail-low-empty
     ((t (:foreground ,darkgrey))))
   `(gnus-group-news-1
     ((t (:foreground ,green :weight bold))))
   `(gnus-group-news-1-empty
     ((t (:foreground ,green))))
   `(gnus-group-news-2
     ((t (:foreground ,yellow :weight bold))))
   `(gnus-group-news-2-empty
     ((t (:foreground ,yellow))))
   `(gnus-group-news-3
     ((t (:foreground ,fg :weight bold))))
   `(gnus-group-news-3-empty
     ((t (:foreground ,fg))))
   `(gnus-group-news-4
     ((t (:foreground ,darkgrey :weight bold))))
   `(gnus-group-news-4-empty
     ((t (:foreground ,darkgrey))))
   `(gnus-group-news-5
     ((t (:foreground ,darkgrey :weight bold))))
   `(gnus-group-news-5-empty
     ((t (:foreground ,darkgrey))))
   `(gnus-group-news-6
     ((t (:foreground ,darkgrey :weight bold))))
   `(gnus-group-news-6-empty
     ((t (:foreground ,darkgrey))))
   `(gnus-group-news-low
     ((t (:foreground ,darkgrey :weight bold))))
   `(gnus-group-news-low-empty
     ((t (:foreground ,darkgrey))))
   `(gnus-summary-selected
     ((t (:background ,cyan :foreground ,bg :underline nil))))
   `(gnus-summary-normal-unread
     ((t (:foreground ,black :weight bold))))
   `(gnus-summary-normal-read
     ((t (:foreground ,fg))))
   `(gnus-summary-normal-ticked
     ((t (:foreground ,yellow))))
   `(gnus-summary-normal-ancient
     ((t (:foreground ,fg))))
   `(gnus-summary-cancelled
     ((t (:background unspecified :foreground ,red))))
   `(gnus-header
     ((t (:inherit default))))
   `(gnus-header-name
     ((t (:foreground ,magenta))))
   `(gnus-header-from
     ((t (:foreground ,fg :weight normal))))
   `(gnus-header-content
     ((t (:foreground ,fg :weight normal :slant normal))))
   `(gnus-header-subject
     ((t (:foreground ,black :weight bold))))
   `(gnus-header-newsgroups
     ((t (:foreground ,black :weight bold))))
   `(gnus-cite-attribution
     ((t (:foreground ,green :weight bold :slant normal :underline nil))))
   `(gnus-cite-1
     ((t (:foreground ,green))))
   `(gnus-cite-2
     ((t (:foreground ,yellow))))
   `(gnus-cite-3
     ((t (:foreground ,blue))))
   `(gnus-cite-4
     ((t (:foreground ,magenta))))
   `(gnus-cite-5
     ((t (:foreground ,cyan))))
   `(gnus-cite-6
     ((t (:foreground ,bright-green))))
   `(gnus-cite-7
     ((t (:foreground ,bright-yellow))))
   `(gnus-cite-8
     ((t (:foreground ,bright-blue))))
   `(gnus-cite-9
     ((t (:foreground ,bright-magenta))))
   `(gnus-cite-10
     ((t (:foreground ,bright-cyan))))
   `(gnus-cite-11
     ((t (:foreground ,darkgrey))))
   `(mm-uu-extract
     ((t (:background ,code-block :foreground ,yellow))))

   ;; highlight-changes
   `(highlight-changes
     ((t (:foreground ,yellow1))))
   `(highlight-changes-delete
     ((t (:foreground ,red :underline t))))

   ;; shr
   `(shr-h1
     ((t (:foreground ,black :weight bold :height 1.6))))
   `(shr-h2
     ((t (:foreground ,black :weight bold :height 1.4))))
   `(shr-h3
     ((t (:foreground ,black :weight bold :height 1.2))))
   `(shr-h4
     ((t (:foreground ,black :weight bold :height 1.1))))
   `(shr-h5
     ((t (:foreground ,black :weight bold :height 1.0))))
   `(shr-h6
     ((t (:foreground ,black :weight bold :height 1.0))))

   ;; dictionary
   `(dictionary-word-definition-face
     ((t (:family nil))))
   `(dictionary-reference-face
     ((t (:foreground ,yellow))))

   ;; markdown
   `(markdown-metadata-key-face
     ((t (:foreground ,darkgrey))))
   `(markdown-header-face
     ((t (:foreground ,black :weight bold))))
   `(markdown-header-delimiter-face
     ((t (:foreground ,blue :weight bold))))
   `(markdown-header-rule-face
     ((t (:foreground ,blue :weight bold))))
   `(markdown-code-face
     ((t (:background ,code-block :foreground ,fg :extend t))))
   `(markdown-list-face
     ((t (:foreground ,yellow))))
   `(markdown-markup-face
     ((t (:foreground ,darkgrey))))
   `(markdown-inline-code-face
     ((t (:background ,code-block :foreground ,cyan))))
   `(markdown-language-keyword-face
     ((t (:foreground ,darkgrey))))
   `(markdown-gfm-checkbox-face
     ((t (:foreground ,darkgrey))))

   ;; diff
   `(diff-header
     ((t (:foreground ,blue :weight bold))))
   `(diff-file-header
     ((t (:foreground ,black))))
   `(diff-hunk-header
     ((t (:foreground ,yellow :weight bold))))
   `(diff-added
     ((t (:background ,diff-added-bg :foreground ,diff-added-fg))))
   `(diff-indicator-added
     ((t (:inherit 'diff-added :foreground ,green))))
   `(diff-refine-added
     ((t (:background ,diff-refine-added-bg :foreground ,diff-added-fg))))
   `(diff-removed
     ((t (:background ,diff-removed-bg :foreground ,diff-removed-fg))))
   `(diff-refine-removed
     ((t (:background ,diff-refine-removed-bg :foreground ,diff-removed-fg))))
   `(diff-indicator-removed
     ((t (:inherit 'diff-removed :foreground ,red))))

   ;; vc
   `(vc-dir-header
     ((t (:foreground unspecified :weight bold))))
   `(vc-dir-header-value
     ((t (:foreground unspecified))))
   `(vc-dir-directory
     ((t (:foreground unspecified :weight bold))))
   `(vc-dir-file
     ((t (:foreground ,fg))))
   `(vc-dir-status-up-to-date
     ((t (:foreground ,green))))
   `(vc-dir-status-edited
     ((t (:foreground ,yellow))))
   `(vc-dir-mark-indicator
     ((t (:foreground ,red))))
   `(vc-edited-state
     ((t (:foreground ,yellow))))
   `(vc-conflict-state
     ((t (:foreground ,red))))
   `(vc-locally-added-state
     ((t (:foreground ,cyan))))
   `(vc-locked-state
     ((t (:foreground ,blue))))
   `(vc-missing-state
     ((t (:foreground ,magenta))))
   `(vc-needs-update-state
     ((t (:foreground ,green))))
   `(vc-removed-state
     ((t (:foreground ,red))))
   `(log-edit-header
     ((t (:foreground ,bright-magenta :weight bold))))
   `(log-edit-summary
     ((t (:foreground ,black :weight bold))))
   `(log-edit-headers-separator
     ((,classTTY (:background unspecified))
      (t (:background ,non-text :height 0.1 :extend t))))
   `(log-view-message
     ((t (:foreground ,darkgrey))))
   `(log-view-commit-body
     ((t (:foreground ,fg))))

   ;; git-commit
   `(git-commit-summary
     ((t (:foreground ,black :weight bold))))
   `(git-commit-nonempty-second-line
     ((t (:foreground ,red :weight bold))))

   ;; magit
   `(magit-section-heading
     ((t (:foreground ,yellow :weight bold))))
   `(magit-section-heading-selection
     ((t (:foreground ,bright-yellow))))
   `(magit-section-highlight
     ((t (:background ,hl-line))))
   `(magit-branch-local
     ((t (:foreground ,blue))))
   `(magit-branch-remote
     ((t (:foreground ,green))))
   `(magit-tag
     ((t (:foreground ,bright-yellow))))
   `(magit-dimmed
     ((t (:foreground ,darkgrey))))
   `(magit-hash
     ((t (:foreground ,darkgrey))))
   `(magit-cherry-equivalent
     ((t (:foreground ,magenta))))
   `(magit-cherry-unmatched
     ((t (:foreground ,cyan))))
   `(magit-bisect-bad
     ((t (:foreground ,red))))
   `(magit-bisect-good
     ((t (:foreground ,green))))
   `(magit-bisect-skip
     ((t (:foreground ,yellow))))
   `(magit-diff-hunk-heading
     ((t (:background ,grey1))))
   `(magit-diff-hunk-heading-highlight
     ((t (:background ,grey1 :weight bold))))
   `(magit-diff-context
     ((t (:foreground ,fg))))
   `(magit-diff-context-highlight
     ((t (:background ,grey3))))
   `(magit-diff-added
     ((t (:inherit 'diff-added))))
   `(magit-diff-added-highlight
     ((t (:inherit 'diff-added))))
   `(magit-diff-removed
     ((t (:inherit 'diff-removed))))
   `(magit-diff-removed-highlight
     ((t (:inherit 'diff-removed))))
   `(magit-diff-lines-heading
     ((t (:background ,green :foreground ,black))))
   `(magit-diffstat-added
     ((t (:foreground ,green))))
   `(magit-diffstat-removed
     ((t (:foreground ,red))))
   `(magit-log-author
     ((t (:foreground ,red))))
   `(magit-log-graph
     ((t (:foreground ,darkgrey))))
   `(magit-log-date
     ((t (:foreground ,darkgrey))))
   `(magit-blame-name
     ((t (:foreground ,red))))
   `(magit-blame-date
     ((t (:foreground ,cyan))))
   `(magit-blame-heading
     ((t (:background ,grey1 :foreground ,black))))
   `(magit-blame-margin
     ((t (:background ,grey1 :foreground ,black))))
   `(magit-blame-highlight
     ((t (:background ,grey1 :foreground ,black))))
   `(magit-reflog-amend
     ((t (:foreground ,bright-magenta))))
   `(magit-reflog-merge
     ((t (:foreground ,bright-green))))
   `(magit-reflog-other
     ((t (:foreground ,bright-cyan))))
   `(magit-reflog-reset
     ((t (:foreground ,bright-red))))
   `(magit-reflog-commit
     ((t (:foreground ,bright-green))))
   `(magit-reflog-rebase
     ((t (:foreground ,bright-magenta))))
   `(magit-reflog-remote
     ((t (:foreground ,bright-cyan))))
   `(magit-reflog-checkout
     ((t (:foreground ,bright-blue))))
   `(magit-reflog-cherry-pick
     ((t (:foreground ,bright-green))))

   ;; ediff
   `(ediff-current-diff-A
     ((t (:background ,diff-removed-bg))))
   `(ediff-current-diff-B
     ((t (:background ,diff-added-bg))))
   `(ediff-current-diff-C
     ((t (:background ,diff-changed-bg))))
   `(ediff-current-diff-Ancestor
     ((t (:background ,diff-ancestor-bg))))
   `(ediff-fine-diff-A
     ((t (:background ,diff-refine-removed-bg :foreground ,diff-removed-fg))))
   `(ediff-fine-diff-B
     ((t (:background ,diff-refine-added-bg :foreground ,diff-added-fg))))
   `(ediff-fine-diff-C
     ((t (:background ,diff-refine-changed-bg :foreground ,diff-changed-fg))))
   `(ediff-fine-diff-Ancestor
     ((t (:background ,diff-refine-ancestor-bg :foreground ,diff-ancestor-fg))))
   `(ediff-even-diff-A
     ((t (:background ,grey1))))
   `(ediff-even-diff-B
     ((t (:background ,grey1))))
   `(ediff-even-diff-C
     ((t (:background ,grey1))))
   `(ediff-even-diff-Ancestor
     ((t (:background ,grey1))))
   `(ediff-odd-diff-A
     ((t (:background ,grey1))))
   `(ediff-odd-diff-B
     ((t (:background ,grey1))))
   `(ediff-odd-diff-C
     ((t (:background ,grey1))))
   `(ediff-odd-diff-Ancestor
     ((t (:background ,grey1))))

   ;; smerge
   `(smerge-lower
     ((t (:background ,diff-added-bg))))
   `(smerge-upper
     ((t (:background ,diff-removed-bg))))
   `(smerge-refined-added
     ((t (:background ,green :foreground ,black))))
   `(smerge-refined-removed
     ((t (:background ,red :foreground ,black))))

   ;; epa
   `(epa-mark
     ((t (:foreground ,red :weight bold))))
   `(epa-string
     ((t (:foreground ,green))))
   `(epa-validity-high
     ((t (:foreground ,green :weight bold))))
   `(epa-validity-medium
     ((t (:foreground ,cyan :weight bold))))
   `(epa-validity-low
     ((t (:foreground ,yellow))))
   `(epa-validity-disabled
     ((t (:foreground ,darkgrey))))

   ;; flyspell
   `(flyspell-incorrect
     ((t (:underline (:style wave :color ,bright-red)))))
   `(flyspell-duplicate
     ((t (:underline (:style wave :color ,bright-yellow)))))

   ;; flymake
   `(flymake-error
     ((t (:underline (:style wave :color ,bright-red)))))
   `(flymake-warning
     ((t (:underline (:style wave :color ,bright-yellow)))))

   ;; eglot
   `(eglot-highlight-symbol-face
     ((t (:background ,match))))

   ;; wgrep
   `(wgrep-face
     ((t (:background ,diff-added-bg))))
   `(wgrep-done-face
     ((t (:foreground ,yellow))))
   `(wgrep-file-face
     ((t (:inherit wgrep-face))))
   `(wgrep-delete-face
     ((t (:background ,diff-removed-bg))))
   `(wgrep-reject-face
     ((t (:inherit error))))

   ;; erc
   `(erc-timestamp-face
     ((t (:foreground ,bright-cyan))))
   `(erc-notice-face
     ((t (:foreground ,darkgrey))))
   `(erc-my-nick-face
     ((t (:foreground ,red :weight bold))))
   `(erc-current-nick-face
     ((t (:foreground ,red :weight bold))))
   `(erc-nick-msg-face
     ((t (:foreground ,yellow))))
   `(erc-input-face
     ((t (:foreground ,purple))))
   `(erc-error-face
     ((t (:foreground ,bright-red))))
   `(erc-dangerous-host-face
     ((t (:foreground ,bright-red))))
   `(erc-direct-msg-face
     ((t (:foreground ,yellow))))
   `(erc-button
     ((t (:background unspecified :foreground ,blue :underline t))))
   `(erc-prompt-face
     ((t (:background unspecified :foreground ,bright-magenta :weight bold))))
   `(erc-action-face
     ((t (:background unspecified :foreground ,comment))))
   `(fg:erc-face0
     ((t (:foreground ,black))))
   `(fg:erc-face1
     ((t (:foreground ,red))))
   `(fg:erc-face2
     ((t (:foreground ,green))))
   `(fg:erc-face3
     ((t (:foreground ,yellow))))
   `(fg:erc-face4
     ((t (:foreground ,blue))))
   `(fg:erc-face5
     ((t (:foreground ,magenta))))
   `(fg:erc-face6
     ((t (:foreground ,cyan))))
   `(fg:erc-face7
     ((t (:foreground ,grey))))
   `(fg:erc-face8
     ((t (:foreground ,darkgrey))))
   `(fg:erc-face9
     ((t (:foreground ,bright-red))))
   `(fg:erc-face10
     ((t (:foreground ,bright-green))))
   `(fg:erc-face11
     ((t (:foreground ,bright-yellow))))
   `(fg:erc-face12
     ((t (:foreground ,bright-blue))))
   `(fg:erc-face13
     ((t (:foreground ,bright-magenta))))
   `(fg:erc-face14
     ((t (:foreground ,bright-cyan))))
   `(fg:erc-face15
     ((t (:foreground ,black))))

   ;; rcirc
   `(rcirc-server
     ((t (:foreground ,darkgrey))))
   `(rcirc-timestamp
     ((t (:foreground ,bright-cyan))))
   `(rcirc-prompt
     ((t (:foreground ,bright-magenta :weight bold))))
   `(rcirc-url
     ((t (:background unspecified :foreground ,blue :underline t))))
   `(rcirc-my-nick
     ((t (:foreground ,bright-red :weight bold))))
   `(rcirc-nick-in-message
     ((t (:foreground ,bright-red :weight bold))))
   `(rcirc-other-nick
     ((t (:foreground ,blue))))

   ;; calendar
   `(calendar-month-header
     ((t (:foreground ,black :weight bold))))
   `(calendar-weekday-header
     ((t (:foreground ,blue))))
   `(calendar-weekend-header
     ((t (:foreground ,yellow))))
   `(calendar-today
     ((t (:foreground ,green))))
   `(holiday
     ((t (:background ,bright-magenta :foreground ,bg))))

   ;; elfeed
   `(elfeed-log-date-face
     ((t (:foreground ,yellow))))
   `(elfeed-log-error-level-face
     ((t (:foreground ,red))))
   `(elfeed-log-warn-level-face
     ((t (:foreground ,yellow))))
   `(elfeed-log-info-level-face
     ((t (:foreground ,blue))))
   `(elfeed-log-debug-level-face
     ((t (:foreground ,magenta))))
   `(elfeed-search-date-face
     ((t (:foreground ,darkgrey))))
   `(elfeed-search-title-face
     ((t (:foreground ,black))))
   `(elfeed-search-unread-title-face
     ((t (:foreground ,black :weight bold))))
   `(elfeed-search-feed-face
     ((t (:foreground ,green))))
   `(elfeed-search-tag-face
     ((t (:foreground ,yellow))))
   `(elfeed-search-unread-count-face
     ((t (:foreground ,blue))))
   `(info-menu-star
     ((t (:foreground ,bright-yellow))))

   ;; comint
   `(comint-highlight-prompt
     ((t (:foreground ,bright-magenta :weight bold))))

   ;; eshell
   `(eshell-prompt
     ((t (:foreground ,bright-magenta :weight bold))))
   `(eshell-ls-directory
     ((t (:foreground ,blue :weight bold))))
   `(eshell-ls-symlink
     ((t (:foreground ,cyan :weight bold))))
   `(eshell-ls-executable
     ((t (:foreground ,green :weight bold))))
   `(eshell-ls-cluttern
     ((t (:foreground ,red))))
   `(eshell-ls-archive
     ((t (:foreground ,yellow))))
   `(eshell-ls-backup
     ((t (:foreground ,darkgrey))))
   `(eshell-ls-unreadable
     ((t (:foreground ,non-text))))
   `(eshell-ls-missing
     ((t (:background ,red :foreground ,black))))
   `(eshell-ls-product
     ((t (:foreground ,black))))
   `(eshell-ls-readonly
     ((t (:foreground ,darkgrey))))
   `(eshell-ls-special
     ((t (:foreground ,magenta))))

   ;; eww
   `(eww-form-text
     ((t (:inherit widget-field :box (:foreground ,grey)))))
   `(eww-form-textarea
     ((t (:inherit widget-field))))
   `(eww-form-submit
     ((t (:inherit custom-button))))
   `(eww-form-file
     ((t (:inherit custom-button))))
   `(eww-valid-certificate
     ((t (:foreground ,green))))
   `(eww-invalid-certificate
     ((t (:foreground ,red))))

   ;; emms
   `(emms-playlist-selected-face
     ((t (:foreground ,blue :weight bold))))
   `(emms-playlist-track-face
     ((t (:foreground ,fg))))
   `(emms-browser-track-face
     ((t (:inherit emms-playlist-track-face))))
   `(emms-browser-artist-face
     ((t (:foreground ,blue))))
   `(emms-browser-album-face
     ((t (:foreground ,yellow))))
   `(emms-browser-composer-face
     ((t (:foreground ,cyan))))
   `(emms-browser-performer-face
     ((t (:foreground ,magenta))))
   `(emms-browser-year/genre-face
     ((t (:foreground ,red))))

   ;; vertico
   `(vertico-current
     ((t (:background ,hl-line))))

   ;; orderless
   `(orderless-match-face-0
     ((t (:foreground ,red :weight bold))))
   `(orderless-match-face-1
     ((t (:foreground ,blue :weight bold))))
   `(orderless-match-face-2
     ((t (:foreground ,green :weight bold))))
   `(orderless-match-face-3
     ((t (:foreground ,yellow :weight bold))))

   ;; marginalia
   `(marginalia-key
     ((t (:foreground ,cyan))))
   `(marginalia-date
     ((t (:foreground ,darkgrey))))
   `(marginalia-file-priv-dir
     ((t (:foreground ,darkgrey))))
   `(marginalia-file-priv-link
     ((t (:foreground ,cyan))))
   `(marginalia-file-priv-read
     ((t (:foreground ,magenta))))
   `(marginalia-file-priv-write
     ((t (:foreground ,blue))))
   `(marginalia-file-priv-exec
     ((t (:foreground ,yellow))))

   ;; consult
   `(consult-file
     ((t (:foreground ,darkgrey))))
   `(consult-bookmark
     ((t (:foreground ,comment))))
   `(consult-highlight-match
     ((t (:background ,match))))

   ;; embark
   `(embark-keybinding
     ((t (:foreground ,bright-cyan :weight bold))))

   ;; notmuch
   `(notmuch-message-summary-face
     ((t (:background ,grey2))))
   `(notmuch-tag-face
     ((t (:foreground ,yellow))))
   `(notmuch-tag-unread
     ((t (:foreground ,green))))
   `(notmuch-tag-flagged
     ((t (:foreground ,blue))))
   `(notmuch-search-flagged-face
     ((t (:foreground ,blue))))
   `(notmuch-tag-added
     ((t (:underline ,cyan))))
   `(notmuch-tag-deleted
     ((t (:foreground ,red :strike-through ,red))))

   ;; verb (org based restclient)
   `(verb-http-keyword
     ((t (:foreground ,blue))))
   `(verb-header
     ((t (:foreground ,yellow))))
   `(verb-code-tag
     ((t (:foreground ,cyan))))

   ;; sly
   `(sly-mrepl-output-face
     ((t (:foreground ,fg))))
   `(sly-mrepl-note-face
     ((t (:foreground ,yellow))))
   `(sly-action-face
     ((t (:foreground ,bright-blue :weight bold))))

   ;; corfu
   `(corfu-default
     ((t (:background ,grey3))))
   `(corfu-current
     ((t (:background ,grey1))))
   `(corfu-bar
     ((t (:background ,non-text))))
   `(corfu-border
     ((t (:background ,grey))))

   ;; company
   `(company-tooltip
     ((t (:background ,grey3))))
   `(company-tooltip-common
     ((,class256 (:inherit completions-common-part))))
   `(company-tooltip-selection
     ((t (:background ,grey1))))
   `(company-tooltip-scrollbar-track
     ((t (:background ,grey))))
   `(company-tooltip-scrollbar-thumb
     ((t (:background ,non-text))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face
     ((t (:foreground ,fg))))
   `(rainbow-delimiters-depth-2-face
     ((t (:foreground ,blue))))
   `(rainbow-delimiters-depth-3-face
     ((t (:foreground ,yellow))))
   `(rainbow-delimiters-depth-4-face
     ((t (:foreground ,cyan))))
   `(rainbow-delimiters-depth-5-face
     ((t (:foreground ,magenta))))
   `(rainbow-delimiters-depth-6-face
     ((t (:foreground ,bright-blue))))
   `(rainbow-delimiters-depth-7-face
     ((t (:foreground ,red))))
   `(rainbow-delimiters-depth-8-face
     ((t (:foreground ,bright-cyan))))
   `(rainbow-delimiters-depth-9-face
     ((t (:foreground ,bright-yellow))))

   ;; rst
   `(rst-level-1
     ((t (:background unspecified :foreground ,black :weight bold))))
   `(rst-level-2
     ((t (:background unspecified :foreground ,black :weight bold))))
   `(rst-level-3
     ((t (:background unspecified :foreground ,black :weight bold))))
   `(rst-level-4
     ((t (:background unspecified :foreground ,black :weight bold))))
   `(rst-level-5
     ((t (:background unspecified :foreground ,black :weight bold))))
   `(rst-level-6
     ((t (:background unspecified :foreground ,black :weight bold))))
   `(rst-literal
     ((t (:foreground ,magenta))))
   `(rst-directive
     ((t (:foreground ,purple))))
   `(rst-block
     ((t (:foreground ,red))))
   `(rst-definition
     ((t (:foreground ,green))))

   ;; sh
   `(sh-quoted-exec
     ((t (:foreground ,bright-magenta))))

   ;; tuareg
   `(tuareg-font-lock-error-face
     ((t (:foreground ,bright-red :background ,white :inverse-video t))))
   `(tuareg-font-lock-interactive-error-face
     ((t (:inherit error))))
   `(tuareg-font-double-semicolon-face
     ((t (:foreground ,bright-red))))
   `(tuareg-font-lock-interactive-output-face
     ((t (:foreground ,darkgrey))))
   `(tuareg-font-lock-line-number-face
     ((t (:foreground ,darkgrey))))
   `(tuareg-font-lock-extension-node-face
     ((t (:inherit font-lock-preprocessor-face))))
   `(tuareg-font-lock-interactive-directive-face
     ((t (:inherit font-lock-keyword-face))))
   `(tuareg-font-lock-governing-face
     ((t (:inherit font-lock-keyword-face))))
   `(tuareg-font-lock-operator-face
     ((t (:inherit font-lock-keyword-face))))

   `(which-key-key-face
     ((t (:inherit help-key-binding))))
   `(which-key-separator-face
     ((t (:foreground ,darkgrey))))
   `(which-key-command-description-face
     ((t (:inherit default))))
   `(which-key-group-description-face
     ((t (:foreground ,bright-magenta))))

   ;; tempel
   `(tempel-default
     ((t (:background ,diff-added-bg))))
   `(tempel-field
     ((t (:background ,diff-added-bg))))
   `(tempel-form
     ((t (:background unspecified))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'sandcastle)
;;; sandcastle-theme.el ends here
