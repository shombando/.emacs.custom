;; Extracted from https://github.com/LionyxML/LEmacs
(use-package prisma-mode
  :defer t
  :mode "\\.prisma?\\'"
  :load-path "prisma-mode/")

(use-package lsp-prisma
  :defer t
  :after (:any prisma-mode)
  :load-path "prisma-mode/")

(use-package lsp-mode
  :straight t
  :defer t
  :init
  (setq read-process-output-max (* 1024 1024 4)) ; hacky
  (require 'prisma-mode)
  (require 'lsp-prisma)
  (add-hook 'before-save-hook #'(lambda () (when (eq major-mode 'prisma-mode)
											 (lsp-format-buffer))))
  (add-hook 'prisma-mode-hook #'lsp-deferred)
(setq lsp-language-id-configuration '((java-mode . "java")
                                          (python-mode . "python")
										  (python-ts-mode . "python")
                                          (gfm-view-mode . "markdown")
                                          (rust-mode . "rust")
                                          (rustic-mode . "rust")
                                          (rust-ts-mode . "rust")
                                          (css-mode . "css")
                                          (sass-mode . "sass")
                                          (xml-mode . "xml")
                                          (c-mode . "c")
                                          (c++-mode . "cpp")
                                          (objc-mode . "objective-c")
                                          (web-mode . "html")
                                          (html-mode . "html")
                                          (sgml-mode . "html")
                                          (mhtml-mode . "html")
                                          (go-mode . "go")
                                          (haskell-mode . "haskell")
                                          (php-mode . "php")
                                          (json-mode . "json")
                                          (js-ts-mode . "javascript")
                                          (js-mode . "javascript")
                                          (rjsx-mode . "javascript")
                                          (javascript-mode . "javascript")
                                          (typescript-mode . "typescript")
                                          (typescript-ts-mode . "typescript")
                                          (tsx-ts-mode . "typescriptreact")
                                          (prisma-mode . "prisma")
                                          (typescriptreact-mode . "typescriptreact")
                                          (ruby-mode . "ruby")
										  (emacs-lisp-mode . nil)
                                          ))
:hook ((python-ts-mode . lsp)
		 (typescript-ts-mode . lsp)
		 (rust-ts-mode . lsp)
		 (css-mode . lsp)
		 (sass-mode . lsp)
		 (web-mode . lsp)
		 (prisma-mode . lsp))
  :ensure t
  :config
  (lsp-inlay-hints-mode)
  (setq lsp-inlay-hint-enable t)

  (setq lsp-enable-links nil)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-python-ms-python-executable "/usr/bin/python3")

  (setq lsp-headerline-breadcrumb-enable-symbol-numbers t)
  (setq lsp-headerline-arrow "▶")
  (setq lsp-headerline-breadcrumb-enable-diagnostics nil)
  (setq lsp-headerline-breadcrumb-icons-enable nil)

  (setq lsp-ui-doc-use-childframe t)

  (setq lsp-log-io nil)   ;; Don't log everything = speed
  (setq lsp-idle-delay 0) ;; If needed, increase to 0.5...
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-keymap-prefix "C-c l")
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)

  (global-set-key (kbd "M-3") 'lsp-ui-peek-find-implementation)
  (global-set-key (kbd "M-4") 'lsp-ui-peek-find-references)
  (global-set-key (kbd "M-5") 'lsp-ui-doc-toggle))


(use-package lsp-ui
  :straight t
  :after (:all lsp)
  :custom
  (lsp-ui-doc-max-width 100)
  (lsp-ui-peek-always-show nil)
  (lsp-ui-doc-alignment 'window)
  (lsp-ui-doc-position 'top) ;; 'at-point 'top 'bottom
  ;; (lsp-ui-doc-show-with-cursor t)
  ;; (lsp-ui-doc-enable t)
  ;; (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-diagnostic-max-line-length 100)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil)
  :config)

(use-package eglot
  :straight t
  :preface
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  :config
  (progn
	(add-hook 'python-ts-mode-hook #'eglot-ensure)
	(add-hook 'js-ts-mode-hook #'eglot-ensure)
	(add-hook 'typescript-ts-mode-hook #'eglot-ensure)
	(add-hook 'typescriptreact-mode-hook #'eglot-ensure)
	(add-hook 'tsx-ts-mode-hook #'eglot-ensure)
	(add-hook 'rust-ts-mode-hook #'eglot-ensure)
	(add-hook 'css-mode-hook #'eglot-ensure)
	(add-hook 'sass-mode-hook #'eglot-ensure)
	(add-hook 'web-mode-hook #'eglot-ensure)
	(add-hook 'prisma-mode-hook #'eglot-ensure)

	(bind-keys :map eglot-mode-map
			   ("C-c l a" . eglot-code-actions)
			   ("C-c l o" . eglot-code-action-organize-imports)
			   ("C-c l r" . eglot-rename)
			   ("C-c l f" . eglot-format)))

  (defun my-enable-flymake-eslint ()
	"Enable eslint if typescript mode"
	(when (or (eq major-mode 'tsx-ts-mode)
			  (eq major-mode 'typescript-ts-mode)
			  (eq major-mode 'javascript-ts-mode)
			  (eq major-mode 'typescriptreact-mode))
	  (flymake-eslint-enable)))

  (add-hook 'eglot-managed-mode-hook #'my-enable-flymake-eslint))
