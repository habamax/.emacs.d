;;; habamax-dev.el  -*- lexical-binding: t; -*-

;;;###autoload
(defun habamax-dev/run-c-file ()
  "Compile and run single c file"
  (interactive)
  (unless (or (file-exists-p "makefile")
              (file-exists-p "Makefile")
              (not buffer-file-name))
    (let ((file-name (file-name-sans-extension buffer-file-name)))
      (compile
       (concat "make -k " (shell-quote-argument file-name)
               " && chmod +x " (shell-quote-argument file-name)
               " && " (shell-quote-argument file-name))))))


;;;###autoload
(defun habamax-dev/run-python-file ()
  "Compile and run single python file"
  (interactive)
  (unless (not buffer-file-name)
    (let ((file-name buffer-file-name))
      (compile
       (concat "python " (shell-quote-argument file-name))))))


(provide 'habamax-dev)
