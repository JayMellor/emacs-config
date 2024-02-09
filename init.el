;; To Fix Documents etc issues run the following:

;; % cd /Applications/Emacs.app/Contents/MacOS
;; % mv Emacs Emacs-orig
;; % ln -s Emacs-x86_64-10_14 Emacs

;; % rm bin
;; % ln -s bin-x86_64-10_14 bin

;; % rm libexec
;; % ln -s libexec-x86_64-10_14 libexec

;; Remove unnecessary defaults
(setq inhibit-startup-message t)
(menu-bar-mode 1) ; set to -1 to disable
(setq visible-bell t)
(when (display-graphic-p)
  (scroll-bar-mode 1) ; set to -1 to disable
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (set-fringe-mode 10))

(setq make-backup-files nil) ; stop creating ~ files
(setq-default tab-width 4)
;; (toggle-frame-maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Initialise package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
						 ("org" . "https://orgmode.org/elpa/")
						 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; -p predicate
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(column-number-mode)
(global-display-line-numbers-mode t)

(use-package exec-path-from-shell
  :init
  (when (display-graphic-p)
    (exec-path-from-shell-initialize)))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
				shell-mode-hook
				term-mode-hook
				eshell-mode-hook))
  (add-hook mode (lambda ()
				   (display-line-numbers-mode 0))))

;; Ivy for completions
(use-package ivy
  ;; enable to hide  :diminish
  :bind (("C-s" . swiper)
		 :map ivy-minibuffer-map
		 ("TAB" . ivy-alt-done)
		 ("C-l" . ivy-alt-done)
		 :map ivy-switch-buffer-map
		 ("C-l" . ivy-done)
		 ("C-d" . ivy-switch-buffer-kill)
		 :map ivy-reverse-i-search-map
		 ("C-k" . ivy-previous-line)
		 ("C-d" . ivy-reverse-i-search-kill))
  :init
  (ivy-mode 1))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 35))

(use-package doom-themes
  :config
  (load-theme
   'doom-solarized-light
   ;; 'doom-dark+
   t))

(use-package all-the-icons)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package general
  ;; add prefixes and definers here
  )

(defun new-shell (buffer-name)
  "Creates a new shell instance named BUFFER-NAME"
  (interactive "sname: ")
  (shell buffer-name))

(general-define-key
 "C-x C-b" 'counsel-switch-buffer
 "C-c s" 'new-shell)

;; evil-mode

;; hydra-mode

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode)
  :init
  (setq counsel-projectile-switch-project-action
		#'counsel-projectile-switch-project-action-find-file))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
								 "diff in same window")
  (magit-define-global-key-bindings 'recommended))

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
		 ("\\.mdx" . markdown-mode))
  :hook
  (markdown-mode-hook . (lambda ()
						  (visual-line-mode 1)))
  :config
  (unbind-key "<backtab>" markdown-mode-map)  ; to remove awkward (markdown-cycle t) behaviour
  :init
  (setq markdown-command "markdown"))

(use-package org
  :config
  (setq org-ellipsis " ."
	;; hide *, / etc
	org-hide-emphasis-markers t)
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files '("~/Documents/Learning and Testing apps/emacs/Tasks.org"))

  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
	  (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "|" "COMPLETED(c)" "CANCELLED(k@)")))

  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))
;; org-agenda-custom-commands

(defun my/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
		      (expand-file-name "~/.emacs.d/Emacs.org"))
    (let ((org-confirm-babel-evaulate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook
		  (lambda ()
			(add-hook 'after-save-hook #'my/org-babel-tangle-config))) ; can this be done with require?

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(use-package smartparens
  :hook ((prog-mode . smartparens-strict-mode)
	 (slime-repl-mode . smartparens-strict-mode))
  :bind (("C-<right>" . sp-forward-slurp-sexp)
	 ("C-<left>" . sp-forward-barf-sexp))
  :config (require 'smartparens-config))

(use-package company
  :init
  (company-mode 1))

(use-package lsp-mode
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  ((python-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration)))

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))


(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package go-mode
  :bind (("C-c C-c" . gofmt))
  :config
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package gorepl-mode
  :hook (go-mode . gorepl-mode))

(use-package slime
  :init
  (when (file-exists-p (expand-file-name "~/.quicklisp/slime-helper.el"))
	(load (expand-file-name "~/.quicklisp/slime-helper.el")))
  (setq inferior-lisp-program "/opt/homebrew/bin/sbcl")
  ;; not needed - see C-c <RET>
  :bind (("C-c m" . slime-macroexpand-1)))

(add-to-list 'load-path "~/.emacs.d/testmode/")
(require 'testmode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(slime company company-mode lsp-pyright lsp-ui typescript-mode lsp-mode smartparens org-bullets forge magit counsel-projectile general doom-themes helpful ivy-rich which-key which-keys rainbow-delimiters use-package doom-modeline counsel command-log-mode))
 '(same-window-regexps nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
