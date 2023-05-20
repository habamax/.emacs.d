;;; habamax-windows.el -- windows settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq display-buffer-alist
      '(("\\*e?shell\\*"
         (display-buffer-in-side-window)
         (window-height . 0.3)
         (side . bottom)
         (slot . -1))
        ("\\*\\(grep\\|compilation\\|godot - .+\\)\\*"
         (display-buffer-in-side-window)
         (window-height . 0.3)
         (side . bottom)
         (slot . 0))
        ("\\*Customize .*\\*"
         (display-buffer-in-side-window)
         (window-width . 0.3)
         (side . right)
         (slot . -1))))

(provide 'habamax-windows)
;;; habamax-windows.el ends here
