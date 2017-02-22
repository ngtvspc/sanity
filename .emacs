;;; package --- Summary


;;; Commentary:



;;; Packages auto-installation
(setq package-list '(ace-window company dracula-theme flycheck smex magit multi-term python-django))

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			  ("marmalade" . "https://marmalade-repo.org/packages/")
			   ("melpa" . "https://melpa.org/packages/")))


(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;; Code:

(add-hook 'after-init-hook #'global-flycheck-mode)

(global-linum-mode t)
(setq column-number-mode t)

(setq-default indent-tabs-mode nil)

(set-face-attribute 'default nil :height 140)

(setq mac-command-modifier 'meta)

(require 'ido)
(ido-mode t)

;;; Backup

(setq make-backup-files t
      vc-make-backup-files t
      backup-by-copying t
      version-control t
      delete-old-versions t
      delete-by-moving-to-trash t
      kept-old-versions 6
      kept-new-versions 9
      auto-save-default t
      auto-save-timeout 20
      auto-save-interval 200
      )

(setq backup-directory-alist `((".*" . "~/.saves")))
(setq auto-save-file-name-transforms `((".*" "~/.saves/" t)))

;;; Python Django Mode
(add-to-list 'load-path "~/.emacs.d/python-django")
(require 'python-django)

;;; Scroll window around cursor
(defun gcm-scroll-down ()
  (interactive)
  (scroll-up 4))

(defun gcm-scroll-up ()
  (interactive)
  (scroll-down 4))

(global-set-key (kbd "M-<down>") 'gcm-scroll-down)
(global-set-key (kbd "M-<up>") 'gcm-scroll-up)

;; Highlight current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "color-235")
(set-face-foreground 'highlight nil)

;; Dracula theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dracula t)

;; Ace-window mode
(global-set-key (kbd "M-[") 'ace-window)



(provide '.emacs)
;;; .emacs ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(global-relative-line-numbers-mode nil)
 '(global-undo-tree-mode t)
 '(package-selected-packages
   (quote
    (python-django multi-term magit flycheck dracula-theme company ace-window)))
 '(show-paren-mode t)
 '(undo-tree-visualizer-diff t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


