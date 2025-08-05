;;; Estética --- configurações estéticas -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; Tema de interface
(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  :config
  (load-theme 'doom-vibrant t)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; Pacote de ícones
(use-package all-the-icons
  :if (display-graphic-p))

;; Linha indicadora do doom
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-icon nil
        doom-modeline-unicode-fallback nil
        doom-modeline-height 25
        doom-modeline-bar-width 4
        doom-modeline-indent-info nil
        doom-modeline-buffer-file-name-style 'truncate-with-project))

;; Barra de abas
(use-package emacs
  :config
  (setq tab-bar-close-button-show nil
        tab-bar-separator " "
        tab-bar-show t
        tab-bar-format '(tab-bar-format-menu-bar
                         tab-bar-format-tabs-groups
                         tab-bar-separator
                         tab-bar-format-align-right
                         tab-bar-format-global))
  (tab-bar-mode 0))

;; Painel para abertura do Emacs
(use-package dashboard
  :init
  (setq dashboard-set-heading-icons nil
        dashboard-set-file-icons nil
        dashboard-icon-type 'all-the-icons
        dashboard-banner-logo-title "Simple X Emacs Session"
        dashboard-startup-banner 'official ; 'logo 'official
        dashboard-show-shortcuts nil
        dashboard-center-content t
        dashboard-vertically-center-content nil
        dashboard-projects-backend 'projectile
        dashboard-items '((projects . 7)
                          (recents . 10)))
  :config
  (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name))
        tab-bar-new-tab-choice dashboard-buffer-name)
  (dashboard-setup-startup-hook))

;; Definição de fonte
(defvar simplex/monospace-font "Go Mono"
  "Editor monospace font definition.")
(defvar simplex/monospace-font-size 90
  "Editor monospace font size definition.")
(defvar simplex/variable-font "Go"
  "Editor variable-pitch font definition.")
(defvar simplex/variable-font-size 95
  "Editor variable-pitch font size definition.")

(defun simplex/set-font-faces ()
  "Set font attributes used when `display-graphic-p' is t."
  (set-face-attribute
    'default nil
    :font simplex/monospace-font
    :height simplex/monospace-font-size)
  (set-face-attribute
    'fixed-pitch nil
    :font simplex/monospace-font
    :height simplex/monospace-font-size)
  (set-face-attribute
    'variable-pitch nil
    :font simplex/variable-font
    :height simplex/variable-font-size :weight 'regular))

(use-package emacs
  :config
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame (simplex/set-font-faces))))
    (simplex/set-font-faces)))

;; Transparência
(use-package emacs
  :config
  (add-to-list 'default-frame-alist '(alpha-background . 100)))

(provide 'aesthetics)

;;; aesthetics.el ends here
