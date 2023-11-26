;;; habamax-emms.el -- music FTW -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(emms-all)
(setq emms-player-list
      (if +IS-WINDOWS+ '(emms-player-mplayer) '(emms-player-mpv)))
(setq emms-info-functions '(emms-info-native))
(setq emms-source-file-default-directory "~/Music/")
(setq emms-playlist-repeat t)

(defun habamax-emms-play-main ()
  (interactive)
  (emms-play-directory-tree "~/Music/main")
  (emms-shuffle)
  (emms-random))

(provide 'habamax-emms)
;;; habamax-emms.el ends here
