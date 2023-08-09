; Startup
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

(setq inhibit-startup-message t)

(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

(setq vc-make-backup-files t)
 
(global-display-line-numbers-mode)
(set-face-attribute  'line-number-current-line nil :foreground "black")
(line-number-mode)
(column-number-mode)

; Ultra-minimalist package manager
(defmacro using (package)
  `(unless (package-installed-p ,package)
     (package-install ,package)))

(using 'ivy)
(using 'multiple-cursors)
(using 'company)
(using 'magit)

(ivy-mode)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

(add-hook 'after-init-hook 'global-company-mode)

(setq-default magit-process-password-prompt-regexps
  '("^\\(Enter \\)?[Pp]assphrase\\( for \\(RSA \\)?key '.*'\\)?: ?$"
    ;; Match-group 99 is used to identify the "user@host" part.
    "^\\(Enter \\)?[Pp]assword\\( for '\\(https?://\\)?\\(?99:.*\\)'\\)?: ?$"
    ;; Pinentry Curses box in the terminal when used with GnuPG
    "Please enter the passphrase for the ssh key"
    "^.*'s password: ?$"
    "^Yubikey for .*: ?$"
    "^Enter PIN for .*: ?$"))

; LaTeX

(defun latex-compile-buffer ()
  (interactive)
  (let ((fname (buffer-file-name (window-buffer (minibuffer-selected-window)))))
    (save-window-excursion
      (start-process "Tectonic" nil "tectonic" fname))))

(defun latex-view-pdf ()
  (interactive)
  (let* ((buf (window-buffer (minibuffer-selected-window)))
	 (fname (file-name-base (buffer-file-name buf))))
    (save-window-excursion
      (start-process "Zathura" nil "zathura" (format "%s.pdf" fname)))))

(add-hook 'LaTeX-mode-hook
          (lambda ()
	    (local-unset-key (kbd "C-c C-c"))
	    (local-set-key (kbd "C-c C-c") #'latex-compile-buffer)))

; Keymaps

(keymap-global-set "C-x C-a" 'switch-to-prev-buffer)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(global-set-key (kbd "C-c C-s") 'mc/mark-all-in-region)
(global-set-key (kbd "C-c C-n") 'mc/mark-next-lines)
(global-set-key (kbd "C-c C-,") 'mc/keyboard-quit)

(global-set-key (kbd "C-c b") #'(lambda ()
				  (interactive)
				  (switch-to-buffer "*scratch*")))
(global-set-key (kbd "C-c C-y") 'copy-region-as-kill)

; Basically a cheap remix of Vim's <f>, <t> and <a>, respectively.
; TODO: Fix <a> to skip in-between pairs

(defun onto-forward (char)
  (interactive "sEnd:")
  (search-forward char))
(defun onto-backward (char)
  (interactive "sEnd:")
  (search-backward char))
(defun upto-forward (char)
  (interactive "sEnd:")
  (onto-forward char)
  (backward-char))
(defun upto-backward (char)
  (interactive "sEnd:")
  (onto-backward char)
  (forward-char))

(defun upto-between (od do)
  (interactive "sStart:\nsEnd:")
  (upto-backward od)
  (set-mark-command nil)
  (upto-forward do))

(defun onto-between (od do)
  (interactive "sStart:\nsEnd:")
  (onto-backward od)
  (set-mark-command nil)
  (onto-forward do))
    
(global-set-key (kbd "C-c C-x C-f") 'onto-forward)
(global-set-key (kbd "C-c C-x C-b") 'onto-backward)
(global-set-key (kbd "C-c C-x f") 'upto-forward)
(global-set-key (kbd "C-c C-x b") 'upto-backward)

(defvar pairs
  '(("(" . ")")
    ("[" . "]")
    ("'" . "'")
    ("\"" . "\"")
    ("`" . "'")
    ("<<" . ">>")
    ("{" . "}")))

(defun onto-matching (char)
  (interactive "sFirst char of pair:")
  (onto-between char (cdr (assoc char pairs))))
(defun upto-matching (char)
  (interactive "sFirst char of pair:")
  (upto-between char (cdr (assoc char pairs))))

(global-set-key (kbd "C-c C-x C-i") 'onto-matching)
(global-set-key (kbd "C-c C-x i") 'upto-matching)

; Custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(magit company ivy multiple-cursors)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
