;;; Base --- ajustes do estado basico do Emacs -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; Acerto de segurança de leitura de variáveis
(setq enable-local-variables :safe)

;; Desativar boas vindas e diálogos gráficos
(setq inhibit-startup-message t
      use-dialog-box nil)

;; Mensagem inicial do buffer de rascunho
(setq initial-scratch-message
      ";; SimpleX: Scratch buffer -*- lexical-binding: t; -*-\n\n")

;; Desativar artefatos gráficos
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)

;;; Espaçamento das bordas laterais
(set-fringe-mode 0)

;; Alerta visual
(setq visible-bell t
      ring-bell-function 'ignore)

;; Habilitar redimensionamento por pixel
(setq frame-resize-pixelwise t)
; (setq window-resize-pixelwise t)

;; Copiar para área de transferência
(setq select-enable-clipboard t
      select-enable-primary nil)

;; Desabilitar C-z
(keymap-global-unset "C-z")

;; ESC para sair do minibuffer
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Acerto de rolagem
(setq scroll-conservatively 101
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed t
      mouse-wheel-follow-mouse 't
      scroll-step 1
      scroll-margin 1)

;; Posicionamento textual
(column-number-mode t)

;; Destacar linha corrente
(global-hl-line-mode t)

;; Desabilitar destaque de linha para alguns modos
(dolist
    (mode '(term-mode-hook shell-mode-hook eshell-mode-hook))
  (add-hook mode (lambda () (hl-line-mode 0))))

;; Indentação
(setq-default electric-indent-inhibit t
              indent-tabs-mode nil
              tab-width 4)
(setq indent-line-function 'insert-tab)

;; Potencializar remoção de texto
(setq backward-delete-char-untabify-method 'hungry)

;; Texto digitado substitui seleção
(delete-selection-mode t)

;; Visualização de pares
(show-paren-mode t)

;; Restaurar posição do cursor desde a última interação com um arquivo
(save-place-mode t)

;; Limites de uso de memória para 'desfazer
(setq undo-limit 800000
      undo-strong-limit 1200000)

;; Recarregar buffer caso haja mudança no arquivo
(global-auto-revert-mode t)

;; Histórico no mini-buffer
(setq history-length 20)
(savehist-mode t)

;; Arquivos de customização para o /tmp
(setq custom-file (make-temp-file "emacs-custom-"))

;; Método de backup dos arquivos
(setq backup-by-copying t)

(provide 'zero)

;;; zero.el ends here
