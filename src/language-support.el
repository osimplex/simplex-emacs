;;; Linguagens --- acerto de recursos para programação -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; Language Server Protocol
(use-package lsp-mode
  :functions lsp-enable-which-key-integration
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :init
  (setq lsp-enable-symbol-highlighting nil
        lsp-server-install-dir
        (concat (getenv "XDG_CACHE_HOME") "/emacs/lsp")
        lsp-session-file
        (concat (getenv "XDG_CACHE_HOME") "/emacs/lsp/lsp-session-v1")
        lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-log-io nil
        lsp-enable-snippet nil
        lsp-keep-workspace-alive nil))

(use-package lsp-ui
  :after lsp
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-position 'bottom))

(use-package lsp-ivy :after lsp)
(use-package lsp-treemacs
  :after lsp
  :init
  (setq treemacs-persist-file
        (concat (getenv "XDG_CACHE_HOME") "/emacs/treemacs-persist")))

;; Sistema de alertas
(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-checker-error-threshold 500))

;; Estabelecer ripgrep como ferramenta de busca
(setq xref-search-program 'ripgrep
      grep-command "rg -nS --noheading")

;; Mecanismo de complemento de texto
(use-package company
  :functions (company-select-previous-or-abort
              company-select-next-or-abort)
  :hook (after-init . global-company-mode)
  :bind
  ("M-/" . #'company-complete)
  ("C-SPC" . #'company-complete)
  :config
  (setq company-require-match nil
        company-minimum-prefix-length 3
        company-tooltip-align-annotations t)
  (define-key company-active-map (kbd "C-k") #'company-select-previous-or-abort)
  (define-key company-active-map (kbd "C-j") #'company-select-next-or-abort))

;; Numeração de linhas
(use-package emacs
  :defines (display-line-numbers-type
            display-line-numbers-width-start)
  :hook ((text-mode conf-mode org-mode prog-mode) . display-line-numbers-mode)
  :config
  (setq display-line-numbers-type 'relative
        display-line-numbers-width-start 6))

;; Indicadores de indentação
(use-package highlight-indent-guides
  :functions highlight-indent-guides--bitmap-line
  :hook
  ((prog-mode . (lambda ()
                  (highlight-indent-guides-mode t)
                  (highlight-indent-guides-auto-set-faces))))
  :config
  (setq highlight-indent-guides-method 'bitmap ; 'character 'bitmap
        highlight-indent-guides-bitmap-function #'highlight-indent-guides--bitmap-line
        highlight-indent-guides-responsive nil ; 'top
        ;; highlight-indent-guides-auto-top-character-face-perc 250
        highlight-indent-guides-auto-character-face-perc 100))

;; Auxiliar de complemento de pares
(use-package paredit
  :hook
  ((emacs-lisp-mode
    lisp-interaction-mode
    lisp-mode
    scheme-mode) . paredit-mode))

;; Indicadores de parênteses
(use-package rainbow-delimiters
  :hook
  ((emacs-lisp-mode
    lisp-interaction-mode
    lisp-mode
    scheme-mode) . rainbow-delimiters-mode))

;; Pacotes para cada linguagem

;; C
(use-package ccls
  :hook ((c-mode c++-mode) . lsp-deferred))

;; Clojure
(use-package clojure-mode
  :mode
  ("\\.boot$" . clojure-mode)
  ("lein-env" . enh-ruby-mode)
  :hook
  ((clojure-mode clojurescript-mode) . lsp-deferred)
  ((clojure-mode clojurescript-mode) . paredit-mode)
  ((clojure-mode clojurescript-mode) . rainbow-delimiters-mode))

(use-package cider
  :hook (cider-repl-mode . paredit-mode)
  :config
  (setq cider-repl-history-file "/tmp/cider-history"
        cider-repl-wrap-history t))

;; Go
(use-package go-mode
  :hook (go-mode . lsp-deferred))

;; Ledger
(use-package ledger-mode
  :mode
  ("\\.ldg$" . ledger-mode)
  ("\\.ledger$" . ledger-mode)
  :config
  (setq ; ledger-accounts-file "<caminho relativo ao arquivo editado>"
        ; ledger-payees-file "<caminho relativo ao arquivo editado>"
        ledger-post-amount-alignment-at :decimal
        ledger-post-amount-alignment-column 63
        ledger-post-auto-align t
        ledger-complete-in-steps nil
        ledger-default-date-format "%m-%d"
        ledger-reconcile-default-commodity "brl"
        ledger-copy-transaction-insert-blank-line-after t))

(use-package evil-ledger
  :after ledger-mode
  :hook
  (ledger-mode . evil-ledger-mode)
  (evil-ledger-mode . (lambda ()
                        (pop evil-motion-state-modes)
                        (add-to-list 'evil-emacs-state-modes 'ledger-report-mode)))
  :config
  (setq evil-ledger-sort-key "S"))

(use-package flycheck-ledger
  :after (flycheck ledger-mode)
  :hook (ledger-mode . flycheck-mode)
  :init
  (with-eval-after-load 'flycheck (require 'flycheck-ledger))
  :config
  (setq flycheck-ledger-pedantic 'check-payees
        flycheck-ledger-explicit t))

(provide 'language-support)

;;; language-support.el ends here
