;; Emacs is not MS Word
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq inhibit-startup-message t)

;; Save backup files elsewhere
(setq backup-directory-alist
      `(("." . ,(expand-file-name (concat user-emacs-directory "backups")))))

(setq vc-make-backup-files t)

;; Utility functions

(defmacro multi-bind (map &rest bindings)
  (dolist (binding bindings)
    `(define-key ,map ,(car binding) ',(cdr binding))))

;; Package management
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(defvar refreshed nil)
(defsubst using (&rest packages)
  "Install the given PACKAGES if they are not already installed"
  (dolist (package packages)
    (unless (package-installed-p package)
      (unless refreshed
	(setq refreshed t)
	(package-refresh-contents))
      (message "Installing %S..." package)
      (package-install package)
      (message "Installed %S" package))))

(using 'magit)

(using 'rustic
       'lsp-mode
       'lsp-ui)

(multi-bind rustic-mode-map
	    ("M-j" . lsp-ui-imenu)
            ("M-?" . lsp-find-references)
            ("C-c C-c l" . flycheck-list-errors)
            ("C-c C-c a" . lsp-execute-code-action)
            ("C-c C-c r" . lsp-rename)
            ("C-c C-c q" . lsp-workspace-restart)
            ("C-c C-c Q" . lsp-workspace-shutdown)
            ("C-c C-c s" . lsp-rust-analyzer-status))

(setq lsp-rust-analyzer-cargo-watch-command "clippy")
(setq lsp-eldoc-render-all t)
(setq lsp-idle-delay 0.6)
(setq lsp-inlay-hint-enable t)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(setq lsp-ui-peek-always-show t)
(setq lsp-ui-sideline-show-hover t)
(setq lsp-ui-doc-enable nil)

(using 'projectile)

(projectile-mode 1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(using 'ivy
       'which-key)
(ivy-mode)
(setq enable-recursive-minibuffers t)

(which-key-mode)

(using 'company)
(add-hook 'after-init-hook 'global-company-mode)

(using 'geiser-mit)

(using 'haskell-mode
       'lsp-haskell)

(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)

;; Line numbering and modeline stuff
(setq display-line-numbers-type 'relative)
(add-hook 'after-init-hook 'global-display-line-numbers-mode)

(setq column-number-mode t)

;; Buffer refresh
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Shell stuff
(defun comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map
              (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))

;; Latex and PDF files
(defun zathura-open-file (path)
  (interactive "f")
  (start-process "zathura" nil "zathura" path))

(defvar latex-compiler "tectonic")
(defun latex-compile ()
  (interactive)
  (start-process "latex-compile" "*LaTeX compilation*"
		 latex-compiler (buffer-file-name)))

(keymap-global-set "C-c c" 'latex-compile)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(haskell-mode lsp-haskell geiser-mit which-key rustic projectile magit lsp-ui ivy company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
