;;; Emacs --- arquivo inicial de configuração do Emacs -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; Carga dos módulos de configuração

(add-to-list 'load-path
             (concat (getenv "XDG_CONFIG_HOME") "/emacs/src"))

(require 'zero)
(require 'module-setup)
(require 'xdg-base)
(require 'extra-resource)
(require 'aesthetics)
(require 'language-support)
(require 'writing-support)
(require 'org-support)
(require 'evil-system)

;; Otimização de coletor de lixo para interação com LSP
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024))

;;; init.el ends here
