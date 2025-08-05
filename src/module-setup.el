;;; Pacman --- configuraao de gerenciamento de pacotes -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; Importar mecanismo
(require 'package)

;; Adição de repositórios
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Iniciar mecanismo de pacotes e atualizar repositórios
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Garantir o use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Importar mecanismo de gerenciamento de pacotes
(require 'use-package)
(setq use-package-always-ensure t)

(provide 'module-setup)

;;; module-setup.el ends here
