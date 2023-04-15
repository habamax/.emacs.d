;;; wildcharm-theme.el --- port of my vim-wildcharm colorscheme -*- lexical-binding: t; -*-
;; Author: Maxim Kim <habamax@gmail.com>

;;; Commentary:

;; Dark theme with mid-contrast colors.
;; Background in GUI has a bit of a bluish tint.
;; TUI has no background defined, i.e. depends on terminal.

;;; Code:

(deftheme wildcharm "Vibrant and playful.")

(let ((classTC '((class color) (min-colors 257)))
      (class256 '((class color) (min-colors 256)))
      (classTTY '((type tty) (min-colors 16)))
      (fg "#d0d0d0")(bg "#1c1f26")
      (black "#000000")(darkgrey "#808080")
      (red "#d75f5f")(bright-red "#ff5f87")
      (green "#00af5f")(bright-green "#00d75f")
      (yellow "#d78700")(bright-yellow "#ffaf00")
      (blue "#0087d7")(bright-blue "#00afff")
      (magenta "#d787d7")(bright-magenta "#ff87ff")
      (cyan "#00afaf")(bright-cyan "#00d7d7")
      (grey "#d0d0d0")(white "#ffffff")
      (color-comment "#707780")
      (color-match-paren "#ff00af")(color-non-text "#585858")
      (color-mode-line-active-1 "#404348")(color-mode-line-inactive-1 "#303338")
      (color-mode-line-active-2 "#444444")(color-mode-line-inactive-2 "#303030")
      (color-menu "#262626")
      (color-fringe "#12161a")(color-header-line "#32363a")
      (color-special "#875fff")(color-hl-line "#3a3d42")
      (color-match "#3a3632")
      (color-diff-added-bg "#3f4f3f")(color-diff-added-bg-tty "#005f00")
      (color-diff-refine-added-bg "#3f6f4f")(color-diff-refine-added-bg-tty "#005f5f")
      (color-diff-added-fg "#afffaf")
      (color-diff-removed-bg "#4f3f3f")(color-diff-removed-bg-tty "#5f0000")
      (color-diff-refine-removed-bg "#6f4f3f")(color-diff-refine-removed-bg-tty "#870000")
      (color-diff-removed-fg "#ffd7d7"))

  (custom-theme-set-faces
   'wildcharm

   ;;; standard faces
   `(default ((,classTTY (:background nil, :foreground nil))
              (t (:background ,bg :foreground ,fg))))
   `(shadow ((,class256 (:foreground ,color-non-text))))
   `(link ((,class256 (:foreground ,fg :underline t))))
   `(link-visited ((,class256 (:foreground ,darkgrey :underline t))))
   `(highlight ((,class256 (:foreground ,black :background ,bright-blue))))
   `(region ((,class256 (:background ,blue :foreground ,bg))))
   `(secondary-selection ((,class256 (:background ,cyan :foreground ,bg))))
   `(trailing-whitespace ((,class256 (:foreground ,red :weight bold))))
   `(line-number ((,class256 (:foreground ,color-non-text))))
   `(line-number-current-line ((,class256 (:foreground ,yellow :weight bold))))
   ;; -line-number-major-tick
   ;; -line-number-minor-tick
   ;; -fill-column-indicator
   `(escape-glyph ((,class256 (:foreground ,color-special))))
   `(homoglyph ((,class256 (:inherit 'escape-glyph))))
   `(nobreak-space ((,class256 (:foreground ,color-special :underline t))))
   `(nobreak-hyphen ((,class256 (:inherit 'escape-glyph))))
   `(mode-line ((,class256 (:background ,color-mode-line-active-1 :foreground ,grey :box (:line-width 1 :color ,color-non-text)))
                (,classTTY (:background ,color-mode-line-active-2 :foreground ,grey))))
   `(mode-line-inactive ((,class256 (:background ,color-mode-line-inactive-1 :foreground ,darkgrey :box (:line-width 1 :color ,black)))
                         (,classTTY (:background ,color-mode-line-inactive-2 :foreground ,darkgrey))))
   `(mode-line-highlight ((,class256 (:background ,black :box (:line-width 1 :color ,color-non-text)))))
   `(mode-line-emphasis ((,class256 (:weight bold))))
   `(mode-line-buffer-id ((,class256 (:weight bold))))
   `(header-line ((,class256 (:foreground ,fg :background ,color-header-line :extend t  :box (:line-width 1 :color ,black)))))
   `(vertical-border ((,class256 (:background ,color-mode-line-inactive-2 :foreground ,color-mode-line-inactive-2))
                      (,classTTY (:background ,color-mode-line-inactive-2 :foreground ,color-mode-line-inactive-2))))
   ;; -window-divider '((t :foreground "gray60"))
   ;; -window-divider-first-pixel
   ;; -window-divider-last-pixel
   ;; -internal-border
   ;; -child-frame-border
   `(minibuffer-prompt ((,class256 (:foreground ,bright-yellow :weight bold))))
   `(fringe ((,classTTY (:foreground ,green :background ,red))
             (,class256 (:background ,color-fringe))))
   ;; -scroll-bar
   `(cursor ((t (:background "#ffffff"))))
   ;; -tool-bar
   `(tab-bar ((,class256 (:background ,color-mode-line-inactive-1 :foreground ,darkgrey))
              (,classTTY (:background ,color-mode-line-inactive-2 :foreground ,darkgrey))))
   `(tab-bar-tab ((,class256 (:background ,color-mode-line-active-1 :foreground ,white :weight bold :box (:line-width 1 :color ,color-non-text)))
                  (,classTTY (:background ,color-mode-line-active-2 :foreground ,white :weight bold))))
   `(tab-bar-tab-inactive ((t (:background nil :foreground ,grey))))
   `(help-key-binding ((,class256 (:foreground ,cyan :background nil :box (:line-width (1 . -1) :color ,cyan)))))
   `(error ((,class256 (:foreground ,bright-red :weight bold))))
   `(warning ((,class256 (:foreground ,yellow :weight bold))))
   `(success ((,class256 (:foreground ,bright-green :weight bold))))
   `(menu ((t (:background ,color-mode-line-inactive-2 :foreground ,grey))))
   `(tty-menu-enabled-face ((t (:background ,color-menu :foreground ,grey))))
   `(tty-menu-disabled-face ((t (:background ,color-menu :foreground ,darkgrey))))
   `(tty-menu-selected-face ((t (:background ,color-mode-line-active-2 :foreground ,grey :weight bold))))
   
   `(show-paren-match ((,class256 :foreground ,color-match-paren :weight bold)))
   `(show-paren-mismatch ((,class256 :foreground ,white :background ,color-match-paren :weight bold)))
   
   ;;; font-lock
   `(font-lock-string-face ((,class256 (:foreground ,bright-green))))
   `(font-lock-comment-face ((,classTC (:foreground ,color-comment))
                             (t (:foreground ,darkgrey))))
   `(font-lock-keyword-face ((,class256 (:foreground ,bright-blue))))
   `(font-lock-preprocessor-face ((,class256 (:foreground ,bright-cyan))))
   `(font-lock-builtin-face ((,class256 (:foreground ,magenta))))
   `(font-lock-type-face ((,class256 (:foreground ,bright-yellow))))
   `(font-lock-function-name-face ((,class256 (:foreground ,bright-magenta))))
   `(font-lock-variable-name-face ((,class256 (:foreground ,cyan))))
   `(font-lock-constant-face ((,class256 (:foreground ,bright-red))))
   `(font-lock-warning-face ((,class256 (:foreground ,yellow :weight bold))))

   ;;; isearch
   `(isearch ((,class256 (:background ,bright-yellow :foreground ,black))))
   `(lazy-highlight ((,class256 (:background ,bright-green :foreground ,black))))
   `(isearch-fail ((,class256 (:background ,bright-red :foreground ,black))))

   ;;; replace.el
   `(match ((,class256 (:background ,color-match))))

   ;;; global-hl-line-mode
   `(hl-line ((,class256 (:foreground nil :background ,color-hl-line))))

   ;;; widget
   `(widget-field ((,class256 (:foreground ,fg :background ,color-header-line :extend t))))
   
   ;;; dired
   `(dired-header ((,class256 (:foreground ,white :weight bold))))
   `(dired-directory ((,class256 (:inherit font-lock-keyword-face :weight bold))))
   `(dired-symlink ((,class256 (:foreground ,bright-cyan :weight bold :inherit nil))))
   `(dired-broken-symlink ((,class256 (:foreground ,white :background ,red))))
   `(dired-special ((,class256 (:foreground ,magenta :inherit nil))))
   `(dired-perm-write ((,class256 (:foreground ,yellow :inherit nil))))

   ;;; completion
   `(icomplete-first-match ((,class256 (:foreground ,bright-green :weight bold))))
   `(icomplete-selected-match ((,class256 (:background ,color-hl-line))))
   `(completions-common-part ((,class256 (:foreground ,bright-blue :weight bold))))
   `(completions-first-difference ((,class256 (:foreground ,bright-magenta))))

   ;;; ido
   `(ido-first-match ((,class256 (:foreground ,bright-green :weight bold))))
   `(ido-only-match ((,class256 (:inherit 'ido-first-match))))
   `(ido-virtual ((,classTC (:foreground ,color-comment))
                  (,class256 (:foreground ,darkgrey))))
   `(ido-subdir ((,class256 (:foreground ,white :weight bold))))
   ;; check how good it is
   `(ido-indicator ((,class256 (:background ,red :foreground ,bright-yellow))))

   ;;; compilation
   `(compilation-mode-line-fail ((,class256 (:foreground ,red :weight bold))))
   `(compilation-mode-line-exit ((,class256 (:foreground ,green :weight bold))))
   `(compilation-line-number ((,classTC (:foreground ,color-comment))
                              (,class256 (:foreground ,darkgrey))))

   ;;; whitespace
   `(whitespace-space ((,class256 (:background nil :foreground ,color-non-text))))
   `(whitespace-line ((,class256 nil)))
   `(whitespace-trailing ((,class256 (:inherit 'trailing-whitespace))))
   `(whitespace-indentation ((,class256 (:inherit 'whitespace-space))))
   `(whitespace-tab ((,class256 (:inherit 'whitespace-space))))
   `(whitespace-empty ((,class256 (:background ,yellow))))

   ;;; vc
   `(log-view-message ((,classTC (:foreground ,color-comment))
                       (,class256 (:foreground ,darkgrey))))
   `(log-view-commit-body ((,class256 (:foreground ,fg))))
   
   ;;; message
   `(message-header-name ((,class256 (:foreground ,magenta))))
   `(message-header-subject ((,class256 (:foreground ,white :weight bold))))
   `(message-header-to ((,class256 (:foreground ,white))))
   `(message-header-other ((,class256 (:foreground ,fg))))
   `(message-header-cc ((,classTC (:foreground ,color-comment))
                        (,class256 (:foreground ,darkgrey))))
   `(message-separator ((,class256 (:foreground ,fg :background ,color-header-line :extend t))))

   ;;; gnus
   `(gnus-button ((,class256 (:underline t))))
   `(gnus-group-mail-1 ((,class256 (:foreground ,bright-green :weight bold))))
   `(gnus-group-mail-1-empty ((,class256 (:foreground ,bright-green))))
   `(gnus-group-mail-2 ((,class256 (:foreground ,bright-yellow :weight bold))))
   `(gnus-group-mail-2-empty ((,class256 (:foreground ,bright-yellow))))
   `(gnus-group-mail-3 ((,class256 (:foreground ,fg :weight bold))))
   `(gnus-group-mail-3-empty ((,class256 (:foreground ,fg))))
   `(gnus-group-mail-4 ((,class256 (:foreground ,darkgrey :weight bold))))
   `(gnus-group-mail-4-empty ((,class256 (:foreground ,darkgrey))))
   `(gnus-group-mail-5 ((,class256 (:foreground ,darkgrey :weight bold))))
   `(gnus-group-mail-5-empty ((,class256 (:foreground ,darkgrey))))
   `(gnus-group-mail-6 ((,class256 (:foreground ,darkgrey :weight bold))))
   `(gnus-group-mail-6-empty ((,class256 (:foreground ,darkgrey))))
   `(gnus-group-mail-low ((,class256 (:foreground ,darkgrey :weight bold))))
   `(gnus-group-mail-low-empty ((,class256 (:foreground ,darkgrey))))
   `(gnus-group-news-1 ((,class256 (:foreground ,bright-green :weight bold))))
   `(gnus-group-news-1-empty ((,class256 (:foreground ,bright-green))))
   `(gnus-group-news-2 ((,class256 (:foreground ,bright-yellow :weight bold))))
   `(gnus-group-news-2-empty ((,class256 (:foreground ,bright-yellow))))
   `(gnus-group-news-3 ((,class256 (:foreground ,fg :weight bold))))
   `(gnus-group-news-3-empty ((,class256 (:foreground ,fg))))
   `(gnus-group-news-4 ((,class256 (:foreground ,darkgrey :weight bold))))
   `(gnus-group-news-4-empty ((,class256 (:foreground ,darkgrey))))
   `(gnus-group-news-5 ((,class256 (:foreground ,darkgrey :weight bold))))
   `(gnus-group-news-5-empty ((,class256 (:foreground ,darkgrey))))
   `(gnus-group-news-6 ((,class256 (:foreground ,darkgrey :weight bold))))
   `(gnus-group-news-6-empty ((,class256 (:foreground ,darkgrey))))
   `(gnus-group-news-low ((,class256 (:foreground ,darkgrey :weight bold))))
   `(gnus-group-news-low-empty ((,class256 (:foreground ,darkgrey))))
   `(gnus-summary-selected ((,class256 (:background ,cyan :foreground ,bg :underline nil))))
   `(gnus-summary-normal-unread ((,class256 (:foreground ,white :weight bold))))
   `(gnus-summary-normal-read ((,class256 (:foreground ,fg))))
   `(gnus-summary-normal-ticked ((,class256 (:foreground ,bright-yellow))))
   `(gnus-summary-normal-ancient ((,class256 (:foreground ,fg))))
   `(gnus-summary-cancelled ((,class256 (:foreground ,red :background nil))))
   `(gnus-header-name ((,class256 (:foreground ,magenta))))
   `(gnus-header-from ((,class256 (:foreground ,fg :weight normal))))
   `(gnus-header-content ((,class256 (:foreground ,fg :weight normal :slant normal))))
   `(gnus-header-subject ((,class256 (:foreground ,white :weight bold))))
   `(gnus-cite-attribution ((,class256 (:foreground ,bright-green :weight bold :slant normal :underline nil))))
   `(gnus-cite-1 ((,class256 (:foreground ,bright-green))))
   `(gnus-cite-2 ((,class256 (:foreground ,bright-yellow))))
   `(gnus-cite-3 ((,class256 (:foreground ,bright-blue))))
   `(gnus-cite-4 ((,class256 (:foreground ,bright-magenta))))
   `(gnus-cite-5 ((,class256 (:foreground ,bright-cyan))))
   `(gnus-cite-6 ((,class256 (:foreground ,green))))
   `(gnus-cite-7 ((,class256 (:foreground ,yellow))))
   `(gnus-cite-8 ((,class256 (:foreground ,blue))))
   `(gnus-cite-9 ((,class256 (:foreground ,magenta))))
   `(gnus-cite-10 ((,class256 (:foreground ,cyan))))
   `(gnus-cite-11 ((,class256 (:foreground ,darkgrey))))

   ;;; shr
   `(shr-h1 ((,class256 (:foreground ,white :weight bold :height 1.6))))
   `(shr-h2 ((,class256 (:foreground ,white :weight bold :height 1.4))))
   `(shr-h3 ((,class256 (:foreground ,white :weight bold :height 1.2))))
   `(shr-h4 ((,class256 (:foreground ,white :weight bold :height 1.1))))
   `(shr-h5 ((,class256 (:foreground ,white :weight bold :height 1.0))))
   `(shr-h6 ((,class256 (:foreground ,white :weight bold :height 1.0))))

   ;;; dictionary
   `(dictionary-word-definition-face ((,class256 (:family nil))))
   `(dictionary-reference-face ((,class256 (:foreground ,yellow))))

   ;;; markdown
   `(markdown-header-face ((,class256 (:foreground ,white :weight bold))))
   `(markdown-header-delimiter-face ((,class256 (:foreground ,bright-blue :weight bold))))
   `(markdown-header-rule-face ((,class256 (:foreground ,bright-blue :weight bold))))
   `(markdown-code-face ((,class256 (:foreground ,fg))))
   `(markdown-list-face ((,class256 (:foreground ,bright-yellow))))
   `(markdown-markup-face ((,classTC (:foreground ,color-comment))
                           (,class256 (:foreground ,darkgrey))))
   `(markdown-inline-code-face ((,class256 (:foreground ,green))))
   `(markdown-language-keyword-face ((,class256 (:foreground ,cyan))))
   `(markdown-gfm-checkbox-face ((,classTC (:foreground ,color-comment))
                                 (,class256 (:foreground ,darkgrey))))

   ;;; diff
   `(diff-header ((,class256 (:foreground ,bright-blue :weight bold))))
   `(diff-file-header ((,class256 (:foreground ,white))))
   `(diff-hunk-header ((,class256 (:foreground ,bright-yellow :weight bold))))
   `(diff-added ((,class256 (:foreground ,color-diff-added-fg :background ,color-diff-added-bg))
                 (,classTTY (:foreground ,color-diff-added-fg :background ,color-diff-added-bg-tty))))
   `(diff-indicator-added ((t (:inherit 'diff-added :foreground ,bright-green))))
   `(diff-refine-added ((,class256 (:foreground ,color-diff-added-fg :background ,color-diff-refine-added-bg))
                        (,classTTY (:foreground ,color-diff-added-fg :background ,color-diff-refine-added-bg-tty))))
   `(diff-removed ((,class256 (:foreground ,color-diff-removed-fg :background ,color-diff-removed-bg))
                   (,classTTY (:foreground ,color-diff-removed-fg :background ,color-diff-removed-bg-tty))))
   `(diff-refine-removed ((,class256 (:foreground ,color-diff-removed-fg :background ,color-diff-refine-removed-bg))
                          (,classTTY (:foreground ,color-diff-removed-fg :background ,color-diff-refine-removed-bg-tty))))
   `(diff-indicator-removed ((t (:inherit 'diff-removed :foreground ,bright-red))))

   ;;; vc
   `(vc-edited-state ((,class256 (:foreground ,yellow))))
   `(vc-conflict-state ((,class256 (:foreground ,red))))
   `(vc-locally-added-state ((,class256 (:foreground ,cyan))))
   `(vc-locked-state ((,class256 (:foreground ,blue))))
   `(vc-missing-state ((,class256 (:foreground ,magenta))))
   `(vc-needs-update-state ((,class256 (:foreground ,green))))
   `(vc-removed-state ((,class256 (:foreground ,bright-red))))
   
   ;;; git-commit
   `(git-commit-summary ((,class256 (:foreground ,white :weight bold))))
   `(git-commit-nonempty-second-line ((,class256 (:foreground ,bright-red :weight bold))))

   ;;; magit
   `(magit-section-heading ((,class256 (:foreground ,bright-yellow :weight bold))))
   `(magit-section-heading-selection ((,class256 (:foreground ,yellow))))
   `(magit-section-highlight ((,class256 (:background ,color-hl-line))))
   `(magit-branch-local ((,class256 (:foreground ,bright-blue))))
   `(magit-branch-remote ((,class256 (:foreground ,bright-green))))
   `(magit-cherry-equivalent ((,class256 (:foreground ,bright-magenta))))
   `(magit-cherry-unmatched ((,class256 (:foreground ,bright-cyan))))
   `(magit-bisect-bad ((,class256 (:foreground ,red))))
   `(magit-bisect-good ((,class256 (:foreground ,green))))
   `(magit-bisect-skip ((,class256 (:foreground ,yellow))))
   `(magit-diff-added ((t (:inherit 'diff-added))))
   `(magit-diff-added-highlight ((t (:inherit 'diff-added))))
   `(magit-diff-removed ((t (:inherit 'diff-removed))))
   `(magit-diff-removed-highlight ((t (:inherit 'diff-removed))))
   `(magit-diff-lines-heading ((,class256 (:foreground ,white :background ,green))))
   `(magit-diffstat-added ((,class256 (:foreground ,bright-green))))
   `(magit-diffstat-removed ((,class256 (:foreground ,bright-red))))

   ;;; smerge
   `(smerge-lower ((,class256 (:background ,color-diff-added-bg))
                   (,classTTY (:background ,color-diff-added-bg-tty))))
   `(smerge-upper ((,class256 (:background ,color-diff-removed-bg))
                   (,classTTY (:background ,color-diff-removed-bg-tty))))
   `(smerge-refined-added ((t (:foreground ,black :background ,green))))
   `(smerge-refined-removed ((t (:foreground ,black :background ,red))))
   
   ;;; erc
   `(erc-timestamp-face ((,classTC (:foreground ,color-comment))
                         (,class256 (:foreground ,darkgrey))))
   `(erc-notice-face ((,classTC (:foreground ,color-comment))
                      (,class256 (:foreground ,darkgrey))))
   `(erc-nick-default-face ((,class256 (:foreground ,blue))))
   `(erc-current-nick-face ((,class256 (:foreground ,bright-red))))
   `(erc-nick-msg-face ((,class256 (:foreground ,bright-yellow))))
   `(erc-input-face ((,class256 (:foreground ,bright-green))))
   `(erc-error-face ((,class256 (:foreground ,red))))
   `(erc-dangerous-host-face ((,class256 (:foreground ,red))))
   `(erc-direct-msg-face ((,class256 (:foreground ,yellow))))
   `(erc-button ((,class256 (:foreground nil :background nil :underline t))))
   `(erc-prompt-face ((,class256 (:foreground ,bright-magenta :background nil :weight bold))))
   `(erc-action-face ((,class256 (:foreground ,cyan :background nil :weight bold))))
   `(fg:erc-color-face0 ((,class256 (:foreground ,black))))
   `(fg:erc-color-face1 ((,class256 (:foreground ,red))))
   `(fg:erc-color-face2 ((,class256 (:foreground ,green))))
   `(fg:erc-color-face3 ((,class256 (:foreground ,yellow))))
   `(fg:erc-color-face4 ((,class256 (:foreground ,blue))))
   `(fg:erc-color-face5 ((,class256 (:foreground ,magenta))))
   `(fg:erc-color-face6 ((,class256 (:foreground ,cyan))))
   `(fg:erc-color-face7 ((,class256 (:foreground ,grey))))
   `(fg:erc-color-face8 ((,class256 (:foreground ,darkgrey))))
   `(fg:erc-color-face9 ((,class256 (:foreground ,bright-red))))
   `(fg:erc-color-face10 ((,class256 (:foreground ,bright-green))))
   `(fg:erc-color-face11 ((,class256 (:foreground ,bright-yellow))))
   `(fg:erc-color-face12 ((,class256 (:foreground ,bright-blue))))
   `(fg:erc-color-face13 ((,class256 (:foreground ,bright-magenta))))
   `(fg:erc-color-face14 ((,class256 (:foreground ,bright-cyan))))
   `(fg:erc-color-face15 ((,class256 (:foreground ,white))))

   ;;; rcirc
   `(rcirc-server ((,classTC (:foreground ,color-comment))
                   (,class256 (:foreground ,darkgrey))))
   `(rcirc-timestamp ((,classTC (:foreground ,color-comment))
                      (,class256 (:foreground ,darkgrey))))
   `(rcirc-prompt ((,class256 (:foreground ,bright-magenta))))
   `(rcirc-url ((,class256 (:foreground nil :background nil :underline t))))
   `(rcirc-my-nick ((,class256 (:foreground ,bright-red :weight bold))))
   `(rcirc-nick-in-message ((,class256 (:foreground ,bright-red :weight bold))))
   `(rcirc-other-nick ((,class256 (:foreground ,bright-yellow))))

   ;;; elfeed
   `(elfeed-log-date-face ((,class256 (:foreground ,yellow))))
   `(elfeed-log-error-level-face ((,class256 (:foreground ,red))))
   `(elfeed-log-warn-level-face ((,class256 (:foreground ,yellow))))
   `(elfeed-log-info-level-face ((,class256 (:foreground ,blue))))
   `(elfeed-log-debug-level-face ((,class256 (:foreground ,magenta))))
   `(elfeed-search-date-face ((,classTC (:foreground ,color-comment))
                              (,class256 (:foreground ,darkgrey))))
   `(elfeed-search-title-face ((,class256 (:foreground ,grey))))
   `(elfeed-search-unread-title-face ((,class256 (:foreground ,white :weight bold))))
   `(elfeed-search-feed-face ((,class256 (:foreground ,green))))
   `(elfeed-search-tag-face ((,class256 (:foreground ,bright-yellow))))
   `(elfeed-search-unread-count-face ((,class256 (:foreground ,bright-blue))))

   ;;; eshell
   `(eshell-prompt ((,class256 (:foreground ,bright-magenta :weight bold))))
   `(eshell-ls-directory ((,class256 (:foreground ,bright-blue :weight bold))))
   `(eshell-ls-symlink ((,class256 (:foreground ,bright-cyan :weight bold))))
   `(eshell-ls-executable ((,class256 (:foreground ,bright-green :weight bold))))
   `(eshell-ls-clutter ((,class256 (:foreground ,red))))
   `(eshell-ls-archive ((,class256 (:foreground ,yellow))))
   `(eshell-ls-backup ((,classTC (:foreground ,color-comment))
                       (,class256 (:foreground ,darkgrey))))
   `(eshell-ls-unreadable ((,class256 (:foreground ,color-non-text))))
   `(eshell-ls-missing ((,class256 (:foreground ,white :background ,red))))
   `(eshell-ls-product ((,class256 (:foreground ,white))))
   `(eshell-ls-readonly ((,classTC (:foreground ,color-comment))
                         (,class256 (:foreground ,darkgrey))))
   `(eshell-ls-special ((,class256 (:foreground ,magenta))))

   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'wildcharm)
;;; wilcharm-theme.el ends here
