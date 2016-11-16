;;; package --- Summary


;;; Commentary:



;;; Code:
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(add-hook 'after-init-hook #'global-flycheck-mode)

(global-linum-mode t)
(setq column-number-mode t)

(setq-default indent-tabs-mode nil)

;;; Function for relative-line-numbers
;;; Not being used currently
(defun banana (linenumber)
  "Return a string representation of an integer for displaying line numbers.
LINENUMBER - an integer"
  (cond ((= 0 linenumber) (nth 1 (split-string (what-line))))
	((> 10 (abs linenumber)) (concat "0" (number-to-string (abs linenumber))))
	((number-to-string (abs linenumber)))))



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

(global-set-key (kbd "ESC <down>") 'gcm-scroll-down)
(global-set-key (kbd "ESC <up>") 'gcm-scroll-up)

;; Highlight current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "color-235")
(set-face-foreground 'highlight nil)

;; Dracula theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dracula t)


(provide '.emacs)
;;; .emacs ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(global-relative-line-numbers-mode nil)
 '(global-undo-tree-mode t)
 '(show-paren-mode t)
 '(undo-tree-visualizer-diff t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
