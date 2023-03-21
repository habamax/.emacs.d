;;; wildcharm-theme.el -- port of my vim-wildcharm colorscheme
;;; Author: Maxim Kim <habamax@gmail.com>


(deftheme wildcharm "Vibrant and playful.")

(let ((class256 '((class color) (min-colors 256)))
      (classTTY '((type tty) (min-colors 16)))
      (fg "#d0d0d0")(bg "#1c1f26")
      (color00 "#000000")(color08 "#767676")
      (color01 "#d7005f")(color09 "#ff5f87")
      (color02 "#00af5f")(color10 "#00d75f")
      (color03 "#d78700")(color11 "#ffaf00")
      (color04 "#0087d7")(color12 "#00afff")
      (color05 "#d787d7")(color13 "#ff87ff")
      (color06 "#00afaf")(color14 "#00d7d7")
      (color07 "#d0d0d0")(color15 "#ffffff")
      (colorMP "#ff00af")(colorNT "#585858")
      (colorMLA1 "#404348")(colorMLI1 "#303338")
      (colorMLA2 "#444444")(colorMLI2 "#303030")
      (colorFr "#12161a")
      (colorSP "#875fff")(colorHL "#3a3d42"))

  (custom-theme-set-faces
   'wildcharm

   `(default ((,classTTY (:background nil, :foreground nil))
              (t (:background ,bg :foreground ,fg))))
   `(cursor ((t (:background "#ffffff"))))

   ;; UI
   `(minibuffer-prompt ((,class256 (:foreground ,color11 :weight bold))))
   `(mode-line ((,class256 (:background ,colorMLA1 :foreground ,color07 :box (:line-width 1 :color ,color00)))
                (,classTTY (:background ,colorMLA2 :foreground ,color07))))
   `(mode-line-inactive ((,class256 (:background ,colorMLI1 :foreground ,color08 :box (:line-width 1 :color ,color00)))
                         (,classTTY (:background ,colorMLI2 :foreground ,color08))))
   `(vertical-border ((,class256 (:foreground ,color08))))
   `(fringe ((,class256 (:background ,colorFr))
             (,classTTY (:background ,color00))))
   `(highlight ((,class256 (:foreground ,color00 :background ,color12))))
   `(hl-line ((,class256 (:foreground nil :background ,colorHL))))
   
   `(isearch ((,class256 (:background ,color11 :foreground ,color00))))
   `(lazy-highlight ((,class256 (:background ,color10 :foreground ,color00))))
   `(isearch-fail ((,class256 (:background ,color09 :foreground ,color00))))
   `(match ((,class256 (:background ,color06 :foreground ,color00))))

   `(region ((,class256 (:background ,color04 :foreground ,bg))))

   `(line-number ((,class256 (:foreground ,colorNT))))
   `(line-number-current-line ((,class256 (:foreground ,color03 :weight bold))))
   `(warning ((,class256 (:foreground ,color03 :weight bold))))
   `(error ((,class256 (:foreground ,color09 :weight bold))))
   `(success ((,class256 (:foreground ,color10 :weight bold))))
   `(shadow ((,class256 (:foreground ,colorNT))))
   `(trailing-whitespace ((,class256 (:foreground ,color01 :weight bold))))

   ;; syntax
   `(font-lock-string-face ((,class256 (:foreground ,color10))))
   `(font-lock-comment-face ((,class256 (:foreground ,color08))))
   `(font-lock-keyword-face ((,class256 (:foreground ,color12))))
   `(font-lock-preprocessor-face ((,class256 (:foreground ,color14))))
   `(font-lock-builtin-face ((,class256 (:foreground ,color05))))
   `(font-lock-type-face ((,class256 (:foreground ,color11))))
   `(font-lock-function-name-face ((,class256 (:foreground ,color13))))
   `(font-lock-variable-name-face ((,class256 (:foreground ,color06))))
   `(font-lock-constant-face ((,class256 (:foreground ,color09))))

   `(font-lock-warning-face ((,class256 (:foreground ,color03 :weight bold))))

   ;; parenthesis and pairs
   `(show-paren-match ((,class256 :foreground ,colorMP :weight bold)))

   ;; links
   `(link ((,class256 (:foreground ,fg :underline (:color, fg)))))
   `(link-visited ((,class256 (:foreground ,color07 :underline (:color ,color07)))))

   ;; dired
   `(dired-directory ((,class256 (:inherit font-lock-keyword-face :weight bold))))

   ;; ido
   `(ido-only-match ((,class256 (:foreground ,color02 :weight bold))))
   `(ido-first-match ((,class256 (:foreground ,color10 :weight bold))))
   `(ido-virtual ((,class256 (:foreground ,color08))))
   `(ido-subdir ((,class256 (:foreground ,color15 :weight bold))))
   ; check how good it is
   `(ido-indicator ((,class256 (:background ,color01 :foreground ,color11))))

   ;; compilation
   `(compilation-mode-line-fail ((,class256 (:foreground ,color01 :weight bold))))
   `(compilation-mode-line-exit ((,class256 (:foreground ,color02 :weight bold))))
   `(compilation-line-number ((,class256 (:foreground ,color08))))
   `(compilation-line-number ((,class256 (:foreground ,color08))))

   ;; whitespace
   `(whitespace-space ((,class256 (:background nil :foreground ,colorNT))))
   `(whitespace-line ((,class256 nil)))
   `(whitespace-trailing ((,class256 (:inherit 'trailing-whitespace))))
   `(whitespace-indentation ((,class256 (:inherit 'whitespace-space))))
   `(whitespace-tab ((,class256 (:inherit 'whitespace-space))))
   `(whitespace-empty ((,class256 (:background ,color03))))

   ;; markdown
   `(markdown-header-face ((,class256 (:foreground ,color15 :weight bold))))
   `(markdown-header-delimiter-face ((,class256 (:foreground ,color12 :weight bold))))
   `(markdown-header-rule-face ((,class256 (:foreground ,color12 :weight bold))))
   `(markdown-code-face ((,class256 (:foreground ,fg))))
   `(markdown-list-face ((,class256 (:foreground ,color11))))
   `(markdown-markup-face ((,class256 (:foreground ,color08))))
   `(markdown-inline-code-face ((,class256 (:foreground ,color02))))
   `(markdown-language-keyword-face ((,class256 (:foreground ,color06))))
   `(markdown-gfm-checkbox-face ((,class256 (:foreground ,color08))))

   ;; diff
   `(diff-header ((,class256 (:foreground ,color12 :weight bold))))
   `(diff-file-header ((,class256 (:foreground ,color15))))
   `(diff-hunk-header ((,class256 (:foreground ,color11 :weight bold))))
   
   ;; git-commit
   `(git-commit-summary ((,class256 (:foreground ,color15 :weight bold))))
   `(git-commit-nonempty-second-line ((,class256 (:foreground ,color09 :weight bold))))
   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'wildcharm)
