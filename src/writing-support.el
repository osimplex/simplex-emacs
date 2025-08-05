;;; Escrita --- adição de recursos para produção de texto -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; Verificador ortográfico

;; Base
;; (setenv "DICPATH"
;;         (concat (getenv "XDG_DATA_HOME") "/hunspell/dictionary"))
(use-package ispell
  :init
  ;; (setq ispell-program-name "hunspell"))
  (setq ispell-program-name "aspell"))

;; Detecção de idioma de arquivo
(use-package guess-language
  :after ispell
  :functions guess-language
  :hook (text-mode . guess-language-mode)
  :init
  (setq guess-language-langcodes
        '((pt . ("pt_BR" nil "BR" "Portuguese"))
          (it . ("it" "Italian" "IT" "Italian"))
          (fr . ("fr_FR" "French" "FR" "French"))
          (en . ("en_US" "English" "US" "English"))))
  :config
  (setq guess-language-languages '(pt fr it en)
        guess-language-min-paragraph-length 35))

;; Destaque de ortografia
(use-package flycheck-aspell
  :after ispell
  :functions flycheck-mode-on-safe ; Definido em flycheck.el
  :config
  (add-to-list 'flycheck-checkers 'tex-aspell-dynamic)
  (add-to-list 'flycheck-checkers 'markdown-aspell-dynamic)
  (flycheck-aspell-define-checker "LaTeX"
    "LaTeX" ("--add-filter" "tex") (LaTeX-mode))
  (add-to-list 'flycheck-checkers 'LaTeX-aspell-dynamic)
  (flycheck-aspell-define-checker "org"
    "Org" ("--add-filter" "url") (org-mode))
  (add-to-list 'flycheck-checkers 'org-aspell-dynamic))

;; Gerenciar linhas longas
(global-visual-line-mode t)

;; Melhoria para o visual-line-mode
(use-package adaptive-wrap
  :hook ((text-mode LaTeX-mode org-mode) . adaptive-wrap-prefix-mode)
  :config
  (setq adaptive-wrap-extra-indent 1))

;; Pacotes para cada linguagem

;; Markdown
(use-package markdown-mode
  :mode
  ("README\\.md$" . gfm-mode)
  :hook
  (markdown-mode . (lambda () (interactive)
                     (guess-language)
                     (flycheck-mode-on-safe)))
  :init
  (setq markdown-command "markdown"
        markdown-asymmetric-header t
        markdown-header-scaling t
        markdown-header-scaling-values '(1.7 1.4 1.2 1.1 1.0 1.0)
        markdown-enable-math nil))

;; LaTeX
(use-package tex
  :ensure auctex
  :mode ("\\.tex$" . LaTeX-mode)
  :mode ("\\.bib$" . LaTeX-mode)
  :hook (LaTeX-mode . TeX-PDF-mode)
  :config
  (setq TeX-auto-save t
        TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-to-list 'TeX-view-program-selection '(output-pdf "Zathura")))

(use-package lsp-latex
  :after tex
  :hook (LaTeX-mode . lsp-deferred))

(provide 'writing-support)

;;; writing-support.el ends here
