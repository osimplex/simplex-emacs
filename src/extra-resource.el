;;; Extras --- adição de recursos para uso geral -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; Aprimoramento de mini-buffer
(use-package ivy
  :functions ivy-format-function-line
  :config
  (setq ivy-wrap t
        ivy-count-format "(%d/%d) "
        ivy-extra-directories nil
        ivy-use-virtual-buffers nil
        ivy-virtual-abbreviate 'full)
  (setf (alist-get 't ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-mode t))
(use-package swiper :bind ("M-I" . 'swiper))
(use-package counsel
  :config
  (setq counsel-find-file-occur-use-find t)
  (counsel-mode t))

(use-package ivy-rich
  :config
  (setq ivy-rich-path-style 'abbrev
        ivy-rich-parse-remote-buffer nil)
  (ivy-rich-mode t))

(use-package consult :bind ("M-O" . 'consult-outline))

(use-package emacs
  :config
  (setq enable-recursive-minibuffers t
        read-extended-command-predicate #'command-completion-default-include-p))

;; Sugestões para complemento de atalho de teclas
(use-package which-key
  :config
  (setq which-key-idle-delay 0.3)
  (which-key-mode t))

;; Explorador de arquivos
(use-package emacs
  :defines dired-kill-when-opening-new-dired-buffer
  :config
  (setq dired-kill-when-opening-new-dired-buffer t))

;; Aprimoramento de tela de ajuda
(use-package helpful)

;; Gerenciamento de projetos
(use-package projectile
  :init
  (setq projectile-cache-file
        (concat (getenv "XDG_CACHE_HOME") "/emacs/projectile.cache")
        projectile-known-projects-file
        (concat (getenv "XDG_DATA_HOME") "/emacs/projectile-bookmarks.eld"))
  :config
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  (setq projectile-completion-system 'ivy)
  (projectile-mode t))

(provide 'extra-resource)

;;; extra-resource.el ends here
