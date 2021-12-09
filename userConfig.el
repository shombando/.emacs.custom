(setq user-full-name "Shom Bandopadhaya"
      user-mail-address "shom@bandopadhaya.com")

(use-package org
  :straight t
  :config (setq org-directory "~/org"
		org-pretty-entities-include-sub-superscripts t))

(use-package org-books
  :straight t
  :config (setq org-books-file "~/org/personal/books.org"))


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
	(mapconcat #'identity
		   `(
		     ,(concat "\n* DRAFT " title)
		     ":PROPERTIES:"
		     ,(concat ":EXPORT_FILE_NAME: " fname)
		     ":EXPORT_HUGO_MENU:\n:END:"
		     "%?\n")          ;Place the cursor here finally
		   "\n")))

    (add-to-list 'org-capture-templates
		 '("h"                ;`org-capture' binding + h
		   "Hugo post"
		   entry
		   ;; It is assumed that below file is present in `org-directory'
		   ;; and that it has a "Blog Ideas" heading. It can even be a
		   ;; symlink pointing to the actual location of all-posts.org!
		   (file+olp "~/dev/shom.dev/content.org" "Content")
		   (function org-hugo-new-subtree-post-capture-template)
		   :prepend t))))

(use-package yaml-mode
  :straight t)

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
	  smtpmail-stream-type  'ssl)))
