(setq user-full-name "Shom Bandopadhaya"
	  user-mail-address "shom@bandopadhaya.com")

(use-package yasnippet
  :straight t
  :config (setq yas-snippet-dirs '("~/.emacs/.custom/snippets"))
  :init
  (yas-global-mode 1)
  (yas-reload-all))

(use-package org
  :straight t
  :config (setq org-directory "~/org"
				org-pretty-entities-include-sub-superscripts t))

(use-package org-books
  :straight t
  :config (setq org-books-file "~/org/personal/books.org"))


(use-package yaml-mode
  :straight t)

(use-package ox-hugo
  :straight t
  :config
  ;; Org capture template for Hugo posts
  ;; https://ox-hugo.scripter.co/doc/org-capture-setup/
  (with-eval-after-load 'org-capture
	(defun org-hugo-new-subtree-post-capture-template ()
	  "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
	  (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
			 (fname (concat (format-time-string "%Y%m%d_") (org-hugo-slug title))))
		(shell-command-to-string
		 (concat "mkdir -p ~/dev/shom.dev/images/posts/" fname))
		(mapconcat #'identity
				   `(
					 ,(concat "\n* DRAFT " title)
					 ":PROPERTIES:\n:EXPORT_FILE_NAME: index"
					 ,(concat ":EXPORT_HUGO_BUNDLE: " fname)
					 ,(concat ":EXPORT_HUGO_CUSTOM_FRONT_MATTER: :aliases /s/"
							  (shell-command-to-string
							   (concat "~/dev/shom.dev/crc32Janky.sh " fname)))
					 ,(concat ":EXPORT_HUGO_IMAGES: /posts/" fname "/image.jpg")
					 ":EXPORT_HUGO_MENU:\n:END:"
					 "%?\n")          ;Place the cursor here finally
				   "\n")))

	(defun org-hugo-new-subtree-start-guide-capture-template ()
	  "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
	  (let* ((title (read-from-minibuffer "Start Guide Title: ")) ;Prompt to enter the post title
			 (fname (org-hugo-slug title)))
		(shell-command-to-string
		 (concat "mkdir -p ~/dev/shom.dev/images/start/" fname))
		(mapconcat #'identity
				   `(
					 ,(concat "\n* DRAFT " title)
					 ":PROPERTIES:\n:EXPORT_FILE_NAME: index"
					 ,(concat ":EXPORT_HUGO_BUNDLE: " fname)
					 ,(concat ":EXPORT_HUGO_CUSTOM_FRONT_MATTER: :aliases /s/"
							  (shell-command-to-string
							   (concat "~/dev/shom.dev/crc32Janky.sh " fname)))
					 ,(concat ":EXPORT_HUGO_IMAGES: /start/" fname "/image.jpg")
					 ":EXPORT_HUGO_MENU:\n:END:"
					 "%?\n")          ;Place the cursor here finally
				   "\n")))

	(add-to-list 'org-capture-templates
				 '("s"
				   "Hugo Start Guide"
				   entry
				   (file+olp "~/dev/shom.dev/start.org" "Start")
				   (function org-hugo-new-subtree-start-guide-capture-template)
				   :prepend t))
	(add-to-list 'org-capture-templates
				 '("h"                ;`org-capture' binding + h
				   "Hugo Post"
				   entry
				   (file+olp "~/dev/shom.dev/posts.org" "Content")
				   (function org-hugo-new-subtree-post-capture-template)
				   :prepend t))))

(use-package yaml-mode
  :straight t)

;; from: https://config.phundrak.com/emacs.html#Packages-Configuration-Programming-languages-Flycheckb446fl6184j0
(use-package flycheck
  :straight (:build t)
  :defer t
  :init
  (global-flycheck-mode)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)

  ;; Rerunning checks on every newline is a mote excessive.
  (delq 'new-line flycheck-check-syntax-automatically)
  ;; And don’t recheck on idle as often
  (setq flycheck-idle-change-delay 2.0)

  ;; For the above functionality, check syntax in a buffer that you
  ;; switched to on briefly. This allows “refreshing” the syntax check
  ;; state for several buffers quickly after e.g. changing a config
  ;; file.
  (setq flycheck-buffer-switch-check-intermediate-buffers t)

  ;; Display errors a little quicker (default is 0.9s)
  (setq flycheck-display-errors-delay 1))

(use-package flycheck-popup-tip
  :straight (:build t)
  :after flycheck evil
  :hook (flycheck-mode . flycheck-popup-tip-mode)
  :config
  (setq flycheck-popup-tip-error-prefix "X ")
  (add-hook 'evil-insert-state-entry-hook
			#'flycheck-popup-tip-delete-popup)
  (add-hook 'evil-replace-state-entry-hook
			#'flycheck-popup-tip-delete-popup))

(use-package flycheck-posframe
  :straight (:build t)
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config
  (setq flycheck-posframe-warning-prefix "! "
		flycheck-posframe-info-prefix    "··· "
		flycheck-posframe-error-prefix   "X "))

(unless sb/is-termux
  (use-package mu4e
	:straight nil
	:defer 20 ; Wait until 20 seconds after startup
	:config

	(setq mu4e-change-filenames-when-moving t ; avoid sync conflicts
		  mu4e-update-interval (* 10 60) ; check mail 10 minutes
		  mu4e-compose-format-flowed t ; re-flow mail so it's not hard wrapped
		  mu4e-get-mail-command "mbsync -a"
		  mu4e-maildir "~/mail/proton")

	(setq mu4e-drafts-folder "/proton/Drafts"
		  mu4e-sent-folder   "/proton/Sent"
		  mu4e-refile-folder "/proton/All Mail"
		  mu4e-trash-folder  "/proton/Trash")

	(setq mu4e-maildir-shortcuts
		  '(("/proton/inbox"     . ?i)
			("/proton/Sent"      . ?s)
			("/proton/Trash"     . ?t)
			("/proton/Drafts"    . ?d)
			("/proton/All Mail"  . ?a)))

	(setq message-send-mail-function 'smtpmail-send-it
		  auth-sources '("~/.authinfo") ;need to use gpg version but only local smtp stored for now
		  smtpmail-smtp-server "127.0.0.1"
		  smtpmail-smtp-service 1025
		  smtpmail-stream-type  'ssl))

  (use-package org-msg
	:straight t
	:after mu4e
	:config
	(setq mail-user-agent 'mu4e-user-agent)
	(require 'org-msg)
	(setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
		  org-msg-startup "hidestars indent inlineimages"
		  org-msg-default-alternatives '((new		. (text html))
										 (reply-to-html	. (text html))
										 (reply-to-text	. (text)))
		  org-msg-convert-citation t)
	(org-msg-mode)))

(with-eval-after-load 'doom-modeline
  ;; Mostly using @daviwil's config
  (defun dw/set-tab-bar-faces ()
	(let ((color (face-attribute 'doom-modeline-bar :background nil t)))
	  (set-face-attribute 'tab-bar-tab t :foreground nil :background nil :weight 'semi-bold :underline `(:color ,color) :inherit nil)
	  (set-face-attribute 'tab-bar nil :font "JetBrains Mono Bold" :height 0.95 :underline `(:color ,color) :foreground nil :inherit 'mode-line)))

  (setq tab-bar-close-button-show nil
		tab-bar-format '(dw/set-tab-bar-faces
						 tab-bar-format-menu-bar
						 tab-bar-format-history
						 tab-bar-format-tabs
						 tab-bar-separator
						 tab-bar-format-add-tab
						 tab-bar-format-align-right
						 tab-bar-format-global
						 tab-bar-separator))

  ;; remove battery from doom-modeline
  (doom-modeline-def-modeline 'default
	'(bar window-number modals matches buffer-info remote-host buffer-position word-count parrot selection-info)
	'(objed-state grip debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker))
  (doom-modeline-set-modeline 'default t)
  (add-to-list 'global-mode-string '("" doom-modeline--battery-status))
  (add-to-list 'global-mode-string '("" tracking-mode-line-buffers))

  (display-time-mode 1)
  ;; (display-battery-mode 1)

  (setq tab-bar-separator " | ")

  ;; Redefine tab-bar-format-menu-bar since there's no option for changing the menu text, taken from karthinks.com
  (defun tab-bar-format-menu-bar ()
	"Produce the Menu button for the tab bar that shows the menu bar."
	`((menu-bar menu-item (propertize " ξ " 'face 'tab-bar-tab-inactive)
				tab-bar-menu-bar :help "Menu Bar")))

  (tab-bar-mode t))

(use-package nov
  :straight t
  :config (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))
