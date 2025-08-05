;;; Org --- configurações de recursos associados ao org -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; Org
(use-package org
  :functions (org-present-read-only
              org-display-inline-images
              org-remove-inline-images
              org-fold-show-children
              org-fold-show-entry
              org-overview)
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers nil)
  (custom-set-faces
   (set-face-attribute 'org-document-title nil :height 1.7)
   '(org-level-1 ((t (:inherit outline-1 :height 1.7))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.4))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
   '(org-level-6 ((t (:inherit outline-6 :height 1.0))))))

;; Marcadores
(use-package org-bullets
  :config
  ;; (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))
  (setq org-bullets-bullet-list '("♣" "♠" "❖" "♦" "❖" "♦" "❖")))

;; Org-contrib
;; (use-package org-contrib)

;; Babel
(use-package emacs
  :after org
  :config
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (clojure . t))))

;; Centralizador de área utilizável
(use-package visual-fill-column
  :init
  (setq visual-fill-column-width 90
        visual-fill-column-center-text t))

;; Apresentações com documentos Org
(defun simplex/org-present-start ()
  "Build a proper presentation environment."
  (setq-local face-remapping-alist
              '((default (:height 1.8) default)
                (header-line (:height 3.0) default)
                (org-document-title (:height 2.8) org-document-title)))
  (setq header-line-format " ")
  (setq org-hide-emphasis-markers t)
  (org-bullets-mode t)
  (org-display-inline-images)
  (visual-fill-column-mode t)
  (display-line-numbers-mode 0)
  (org-present-read-only))

(defun simplex/org-present-end ()
  "Return to normal config after presentation."
  (setq-local face-remapping-alist nil)
  (setq org-hide-emphasis-markers nil)
  (setq header-line-format nil)
  (org-bullets-mode nil)
  (org-remove-inline-images)
  (visual-fill-column-mode 0)
  (display-line-numbers-mode t))

(defun simplex/org-present-prepare-slide (_ _)
  "Collapse topics for next slide."
  (org-overview)
  (org-fold-show-entry)
  (org-fold-show-children))

(use-package org-present
  :hook
  (org-present-mode  . simplex/org-present-start)
  (org-present-mode-quit . simplex/org-present-end)
  :config
  (setq org-present-startup-folded nil
        org-present-hide-stars-in-headings nil)
  (define-key org-present-mode-keymap (kbd "C-c n") 'org-present-next)
  (define-key org-present-mode-keymap (kbd "C-c p") 'org-present-prev)
  (add-hook 'org-present-after-navigate-functions 'simplex/org-present-prepare-slide))

(provide 'org-support)

;;; org-support.el ends here
