(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; https://github.com/emacs-evil/evil-collection
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  ;; https://www.reddit.com/r/spacemacs/comments/6p3w0l/making_q_not_kill_emacs/
  (evil-ex-define-cmd "q" 'kill-this-buffer)
  (evil-ex-define-cmd "quit" 'evil-quit))
(use-package evil-collection
  :after evil
  :custom (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(use-package org
  :ensure t)

(use-package evil-org
  :ensure t
  :after (org evil)
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package counsel
  :ensure t
  :config
  (ivy-mode 1)
  (counsel-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (global-set-key (kbd "C-c k") 'counsel-rg)
  ;; https://www.reddit.com/r/emacs/comments/e02lup/ivy_swiper_doesnt_let_me_rename_or_save_a_file/
  (setq ivy-use-selectable-prompt t))

(use-package rg
  :ensure t)

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-switch-project-action #'projectile-dired))

;; https://www.sandeepnambiar.com/my-minimal-emacs-setup/
(use-package smartparens
  :ensure t
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)
    (show-paren-mode t)))

(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

(use-package dired-sidebar
  :bind ([f8] . dired-sidebar-toggle-sidebar)
  :commands (dired-sidebar-toggle-sidebar)
  :config (setq dired-sidebar-width 60))

(use-package json-mode)

(use-package yaml-mode
  :mode "\\.yml\\'")

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package undo-tree)

(use-package org-journal
  :defer t
  :init
  (setq org-journal-prefix-key "C-c j")
  :config
  (setq org-journal-file-format "%Y%m%d.org")
  (setq org-journal-file-type 'weekly)
  (setq org-journal-date-format "%A, %d %B %Y"))

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package simpleclip
  :config
  (simpleclip-mode 1))

(use-package eyebrowse
  :diminish eyebrowse-mode
  :config (progn
            (define-key eyebrowse-mode-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
            (define-key eyebrowse-mode-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
            (define-key eyebrowse-mode-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
            (define-key eyebrowse-mode-map (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
            (define-key eyebrowse-mode-map (kbd "M-5") 'eyebrowse-switch-to-window-config-5)
            (eyebrowse-mode t)
            (setq eyebrowse-new-workspace t)))

(use-package gruvbox-theme)

(load-theme 'gruvbox-dark-soft t)

;; https://stackoverflow.com/a/23715631
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-insert-state-map (kbd "C-u")
  (lambda ()
    (interactive)
    (evil-delete (point-at-bol) (point))))

(setq visible-bell 1)

;; https://www.emacswiki.org/emacs/BackupDirectory#toc2
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; https://stackoverflow.com/a/12974060
(setq create-lockfiles nil)

(setq evil-emacs-state-modes nil)
(setq evil-insert-state-modes nil)
(setq evil-motion-state-modes nil)

;; https://www.sandeepnambiar.com/my-minimal-emacs-setup/
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)

(global-hl-line-mode +1)
(line-number-mode +1)
(global-display-line-numbers-mode 1)
(column-number-mode t)
(size-indication-mode t)

;; https://emacs.stackexchange.com/a/3008
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; don't polute init.el with custom variables
(setq custom-file "~/.emacs.d/garbage.el")

;; no tabs !!!
(setq-default indent-tabs-mode nil)

;; http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html
(defun open-in-terminal ()
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (let ((process-connection-type nil))
      (start-process "" nil "powershell" "start-process" "powershell"  "-workingDirectory" default-directory)))
   ((string-equal system-type "darwin")
    (shell-command (concat "open -a terminal " (shell-quote-argument (expand-file-name default-directory )))))
   ((string-equal system-type "gnu/linux")
    (let ((process-connection-type nil))
      (start-process "" nil "lxterminal"
                     (concat "--working-directory=" default-directory))))))

(global-set-key (kbd "C-c t") 'open-in-terminal)

;; https://stackoverflow.com/a/296316
;; https://stackoverflow.com/a/3074192
(if (eq system-type 'gnu/linux)
    (set-face-attribute 'default nil :height 140))

;; https://emacs.stackexchange.com/a/245
(global-auto-revert-mode 1)

;; https://emacs.stackexchange.com/a/172
(global-set-key (kbd "C-c r") (lambda ()
                                (interactive)
                                (revert-buffer t t t)
                                (message "buffer is reverted")))

(global-set-key (kbd "C-c C-r") (lambda ()
                                  (interactive)
                                  (eval-buffer)
                                  (message "current buffer evaluated")))

;; https://github.com/syl20bnr/spacemacs/issues/14036#issuecomment-707072523
(global-undo-tree-mode)
(evil-set-undo-system 'undo-tree)

(load "~/.emacs.d/os-specific.el")

;; https://www.emacswiki.org/emacs/YesOrNoP
(defalias 'yes-or-no-p 'y-or-n-p)

;; https://stackoverflow.com/a/3669681
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))
