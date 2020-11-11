(require 'package)
(package-initialize)
(unless package-archive-contents
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-refresh-contents))
(dolist (pkg '(dash projectile org-plus-contrib htmlize))
  (unless (package-installed-p pkg)
    (package-install pkg)))

(require 'org)
(require 'ox-publish)

(defun medivhok-publish ()
  "Publish the blog to HTML."
  (interactive)
  (let ((make-backup-files nil)
        (org-publish-project-alist
         '(("site"
            :base-directory "."
            :base-extension "org"
            :recursive nil
            :publishing-directory "public/"
            :publishing-function org-html-publish-to-html)))
        (user-full-name "Jean Gregory Verret")
        (user-mail-address "gregory.verret@gmail.com")
        (org-export-creator-string
         "<a href=\"https://www.gnu.org/software/emacs/\">Emacs</a> (<a href=\"https://orgmode.org\">Org</a>)")
        (org-html-html5-fancy t)
        (org-html-doctype "html5")
        (org-html-htmlize-output-type 'css))
    (org-publish-all)))

(provide 'publish)
;;; publish.el ends here
