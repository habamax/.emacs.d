;; wildcharm-theme.el -- port of my vim-wildcharm colorscheme
;; Author: Maxim Kim <habamax@gmail.com>


(deftheme wildcharm "Vibrant and playful.")

(let ((fg "#d0d0d0")(bg "#1c1f26")
      (color00 "#000000")(color08 "#767676")
      (color01 "#d7005f")(color09 "#ff5f87")
      (color02 "#00af5f")(color10 "#00d75f")
      (color03 "#d78700")(color11 "#ffaf00")
      (color04 "#0087d7")(color12 "#00afff")
      (color05 "#d787d7")(color13 "#ff87ff")
      (color06 "#00afaf")(color14 "#00d7d7")
      (color07 "#d0d0d0")(color15 "#ffffff")
      (colorMP "#ff00af")(colorNT "#585858")
      (colorLA "#444444")(colorLI "#303030")
      (colorSP "#875fff"))

  (custom-theme-set-faces
   'wildcharm

   `(default ((t (:background ,bg :foreground ,fg))))
   '(cursor ((nil (:background "#ffffff"))))

   ;; UI
   `(minibuffer-prompt ((t (:foreground ,color11 :weight bold))))
   `(mode-line ((t (:background ,colorLA :foreground ,color07 :box (:line-width 1 :color ,color00)))))
   `(mode-line-inactive ((t (:background ,colorLI :foreground ,color08 :box (:line-width 1 :color ,color00)))))
   `(vertical-border ((nil (:foreground ,color08))))
   `(fringe ((nil (:background ,color00))))
   `(highlight ((nil (:foreground ,color00 :background ,color12))))

   `(isearch ((t (:background ,color11 :foreground ,color00))))
   `(lazy-highlight ((t (:background ,color10 :foreground ,color00))))
   `(isearch-fail ((t (:background ,color09 :foreground ,color00))))
   `(match ((t (:background ,color06 :foreground ,color00))))

   `(region ((t (:background ,color04 :foreground ,bg))))

   `(line-number ((t (:foreground ,colorNT))))
   `(line-number-current-line ((t (:foreground ,color03 :weight bold))))
   `(warning ((t (:foreground ,color03 :weight bold))))
   `(error ((t (:foreground ,color09 :weight bold))))
   `(success ((t (:foreground ,color10 :weight bold))))
   `(shadow ((t (:foreground ,colorNT))))
   `(trailing-whitespace ((t (:foreground ,color01 :weight bold))))

   ;; syntax
   `(font-lock-string-face ((t (:foreground ,color10))))
   `(font-lock-comment-face ((t (:foreground ,color08))))
   `(font-lock-keyword-face ((t (:foreground ,color12))))
   `(font-lock-preprocessor-face ((t (:foreground ,color14))))
   `(font-lock-builtin-face ((t (:foreground ,color05))))
   `(font-lock-type-face ((t (:foreground ,color11))))
   `(font-lock-function-name-face ((t (:foreground ,color13))))
   `(font-lock-variable-name-face ((t (:foreground ,color06))))
   `(font-lock-constant-face ((t (:foreground ,color09))))

   `(font-lock-warning-face ((t (:foreground ,color03 :weight bold))))

   ;; parenthesis and pairs
   `(show-paren-match ((t :foreground ,colorMP :weight bold)))

   ;; links
   `(link ((t (:foreground ,fg :underline (:color, fg)))))
   `(link-visited ((t (:foreground ,color07 :underline (:color ,color07)))))

   ;; dired
   '(dired-directory ((t (:inherit font-lock-keyword-face :weight bold))))

   ;; ido
   `(ido-only-match ((t (:foreground ,color10))))
   `(ido-virtual ((t (:foreground ,color08))))
   `(ido-subdir ((t (:foreground ,color08))))

   ;; compilation
   `(compilation-mode-line-fail ((t (:foreground ,color01 :weight bold))))
   `(compilation-mode-line-exit ((t (:foreground ,color02 :weight bold))))
   `(compilation-line-number ((t (:foreground ,color08))))
   `(compilation-line-number ((t (:foreground ,color08))))

   ;; whitespace
   `(whitespace-space ((t (:background nil :foreground ,colorNT))))
   '(whitespace-line ((t nil)))
   '(whitespace-trailing ((t (:inherit 'trailing-whitespace))))
   '(whitespace-indentation ((t (:inherit 'whitespace-space))))
   '(whitespace-tab ((t (:inherit 'whitespace-space))))
   `(whitespace-empty ((t (:background ,color03))))

   ;; markdown
   `(markdown-header-face ((t (:foreground ,color15 :weight bold))))
   `(markdown-code-face ((t (:foreground ,fg))))
   `(markdown-list-face ((t (:foreground ,color09))))
   `(markdown-markup-face ((t (:foreground ,color08))))      

   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'wildcharm)
