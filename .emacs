;;; package --- Summary


;;; Commentary:


;;; Packages auto-installation
(setq package-list '(ace-window company dracula-theme flycheck smex ido-ubiquitous magit multi-term elscreen editorconfig exec-path-from-shell flycheck-pycheckers rjsx-mode))
;; any packages not install automatically may have to be manually installed via browsing package-list-packages
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; set up the $PATH correctly for MacOS and Linux
;; only needs to be done when using the Emacs GUI mode
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; copy the python path
;; (exec-path-from-shell-setenv "PATH" )

;;; Code:

(global-linum-mode t)
(setq column-number-mode t)
(global-auto-revert-mode 1)
(setq-default indent-tabs-mode nil)

(set-face-attribute 'default nil :height 110)

;; (setq mac-command-modifier 'meta)

;; Ido mode

(require 'ido)
(ido-mode t)

(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; The old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

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

;; Elscreen
;; (elscreen-start)



(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'flycheck-pycheckers)
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup))

(require 'editorconfig)
(editorconfig-mode 1)

(provide '.emacs)
;;; .emacs ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-checker-error-threshold 2000)
 '(flycheck-pycheckers-checkers (quote (pylint flake8)))
 '(global-relative-line-numbers-mode nil)
 '(global-undo-tree-mode t)
 '(package-selected-packages
   (quote
    (flycheck-pycheckers smex editorconfig elscreen ido-ubiquitous multi-term magit flycheck dracula-theme company ace-window rjsx-mode)))
 '(show-paren-mode t)
 '(undo-tree-visualizer-diff t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
