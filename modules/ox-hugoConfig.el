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
					 "%?\n\n#+hugo: more")          ;Place the cursor here finally
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
					 ,(concat "\n* DRAFT " title " :start:")
					 ":PROPERTIES:\n:EXPORT_FILE_NAME: index"
					 ,(concat ":EXPORT_HUGO_BUNDLE: " fname)
					 ,(concat ":EXPORT_HUGO_CUSTOM_FRONT_MATTER: :aliases /s/"
							  (shell-command-to-string
							   (concat "~/dev/shom.dev/crc32Janky.sh " fname)))
					 ,(concat ":EXPORT_HUGO_IMAGES: /start/" fname "/image.jpg")
					 ":EXPORT_HUGO_MENU:\n:END:"
					 "%?\n\n#+hugo: more")          ;Place the cursor here finally
				   "\n")))

	(add-to-list 'org-capture-templates
				 '("s"
				   "Hugo Start Guide"
				   entry
				   (file+olp "~/dev/shom.dev/start.org" "Start")
				   (function org-hugo-new-subtree-start-guide-capture-template)
				   :prepend t))
	(add-to-list 'org-capture-templates
				 '("p"                ;`org-capture' binding + h
				   "Hugo Post"
				   entry
				   (file+olp "~/dev/shom.dev/posts.org" "Content")
				   (function org-hugo-new-subtree-post-capture-template)
				   :prepend t))))

(use-package ob-mermaid
  :straight t
  :config
  (setq ob-mermaid-cli-path "/home/shom/.config/nvm/versions/node/v18.16.1/bin/mmdc")
  (org-babel-do-load-languages
		   'org-babel-load-languages
		   '((mermaid .t)
			 (shell . t)
			 (scheme . t))))
