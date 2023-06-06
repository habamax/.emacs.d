;;; habamax-emms.el -- music FTW -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(emms-all)
(setq emms-player-list '(emms-player-mpv))
(setq emms-info-functions '(emms-info-native))
(setq emms-source-file-default-directory "~/Music/")
(setq emms-playlist-repeat t)

(defun habamax/emms-play-main ()
  (interactive)
  (emms-play-directory-tree "~/Music/main")
  (emms-shuffle)
  (emms-random))

(defun habamax/emms-stream-smooth-jazz ()
  (interactive)
  (emms-play-streamlist "http://thejazzgroove.com/itunes.pls"))

(defun habamax/emms-stream-trance ()
  (interactive)
  (emms-play-streamlist "http://www.1.fm/tunein/trance64k.pls"))

(defun habamax/emms-stream-ambient ()
  (interactive)
  (emms-play-streamlist "http://stereoscenic.com/pls/pill-hi-mp3.pls"))

(defun habamax/emms-stream-dnb ()
  (interactive)
  (emms-play-streamlist "http://www.bassdrive.com/BassDrive.m3u"))

(defun habamax/emms-stream-classical ()
  (interactive)
  (emms-play-streamlist "http://www.ibiblio.org/wcpe/wcpe.pls"))

(defun habamax/emms-stream-sheena ()
  (interactive)
  (emms-play-streamlist "http://www.wfmu.org/wfmu.pls"))

(provide 'habamax-emms)
;;; habamax-emms.el ends here
