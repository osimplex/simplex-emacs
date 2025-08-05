;;; Evil --- configuração relativa ao evil-mode -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; Pacotes Evil

;; Instalação e parametrização do pacote e extras
(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-undo-system 'undo-redo)
  (evil-mode t))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(dashboard dired help ibuffer))
  (evil-collection-init))

(use-package evil-org
  :after evil
  :hook (org-mode . evil-org-mode))

(use-package evil-nerd-commenter :after evil)

(use-package evil-multiedit
  :after evil
  :functions evil-multiedit-default-keybinds
  :config
  (evil-multiedit-default-keybinds))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode t))

(use-package evil-tutor)

;; Facilitador para criação de atalhos
(use-package general
  :after evil
  :defines which-key-replacement-alist ; Definido em which-key.el.gz
  :functions nvmap
  :config
  (general-evil-setup t)
  (general-define-key
   "C-x /" 'evilnc-comment-or-uncomment-lines
   "C-x b" 'switch-to-buffer))

;; mapeamento Evil

;; Ajustes Evil
(evil-define-key 'motion 'global
  "G" 'end-of-buffer)

(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
  (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-find-file))

(nvmap 'override
  "C-d" '(evil-scroll-down :which-key "scroll-down")
  "C-u" '(evil-scroll-up :which-key "scroll-up"))

;; Tecla leader
(defvar simplex/leader-states '(normal insert visual emacs)
  "Leader key associated states covering Evil and Emacs modes.")
(general-create-definer simplex/leader
  :states simplex/leader-states
  :prefix "SPC"
  :global-prefix "M-SPC")
(put 'simplex/leader 'function-documentation
     "Leader key, base for custom key bindings.")

;; Emulação de C-c C-h C-x
(simplex/leader
  "h" (general-simulate-key "C-h"
        :docstring "Simulate C-h key pressing."
        :which-key "simulate-C-h")
  "i" (general-simulate-key "C-c"
        :docstring "Simulate C-c key pressing."
        :which-key "simulate-C-c")
  "o" (general-simulate-key "C-x"
        :docstring "Simulate C-x key pressing."
        :which-key "simulate-C-x"))

(provide 'evil-system)

;;; evil-system.el ends here
