;;; XDG --- configuração relativas a conformidade de diretórios -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; Guarda e recuperação
(use-package emacs
  :config
  (let ((backup-dir (concat (getenv "XDG_DATA_HOME") "/emacs/swap/"))
        (autosave-dir (concat (getenv "XDG_DATA_HOME") "/emacs/autosave/")))
    (mkdir backup-dir t)
    (mkdir autosave-dir t)
    (setq backup-directory-alist `(("." . ,backup-dir))
          auto-save-file-name-transforms `((".*" ,autosave-dir t)))))

;; Marcadores
(use-package bookmark
  :init
  (setq bookmark-default-file
        (concat (getenv "XDG_DATA_HOME") "/emacs/bookmarks")))

;; Recentf
(use-package recentf
  :init
  (setq recentf-save-file
        (concat (getenv "XDG_DATA_HOME") "/emacs/recentf"))
  :config
  (setq recentf-max-menu-items 30
        recentf-max-saved-items 30)
  (recentf-mode t))

(provide 'xdg-base)

;;; xdg-base.el ends here
