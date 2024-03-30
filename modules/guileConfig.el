(use-package geiser-guile
  :straight t
  :after org
  :config
  (evil-collection-define-key 'normal 'geiser-repl-mode-map
	"k" 'comint-previous-input
	"j" 'comint-next-input)
  (evil-define-key 'insert geiser-repl-mode-map
	(kbd "<up>") 'comint-previous-input
	(kbd "<down>") 'comint-next-input)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((scheme . t)
	 (shell . t))))
