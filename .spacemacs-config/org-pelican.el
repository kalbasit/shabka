(setq org-publish-project-alist
      '(
        ("base-pages" ;; an identifier
         :base-directory "~/workspace/blog/org" ;; path where I put the articles and pages
         :base-extension "org" ;; export org files
         :publishing-function org-html-publish-to-html ;; use the html publishing method
         :auto-sitemap nil ;; don't generate a sitemap (kind of an index per folder)
         :publishing-directory "~/workspace/blog/public_html/pages" ;; where to publish those files
         :recursive nil ;; recursively publish the files
         :headline-levels 4 ;; Just the default for this project.
         :auto-preamble nil ;; Don't add any kind of html before the content
         :export-with-tags t
         :todo-keywords nil
         ;;:author nil
         :html-doctype "html5" ;; set doctype to html5
         :html-html5-fancy t
         :creator-info nil ;; don't insert creator's info
         :auto-postamble nil ;; Don't add any kind of html after the content
         :html-postamble nil ;; same thing
         :timestamp nil ;;
         :with-toc nil ;;
	 :section-numbers nil ;;
         :exclude-tags ("noexport" "todo")) ;; just in case we don't want to publish some part of the files
        ("blog-static" ;; identifier for static files
         :base-directory "~/workspace/blog/org/posts/files" ;; path where I put the articles and pages
         :publishing-directory "~/workspace/blog/public_html/files" ;; where to publish those files
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :recursive t
         :publishing-function org-publish-attachment ;; method to use for publishing those files
         )
        ("base-posts" ;; an identifier
         :base-directory "~/workspace/blog/org/posts" ;; path where I put the articles and pages
         :base-extension "org" ;; export org files
         :publishing-function org-html-publish-to-html ;; use the html publishing method
         :auto-sitemap nil ;; don't generate a sitemap (kind of an index per folder)
         :publishing-directory "~/workspace/blog/public_html" ;; where to publish those files
         :recursive nil ;; recursively publish the files
         :headline-levels 4 ;; Just the default for this project.
         :auto-preamble nil ;; Don't add any kind of html before the content
         :export-with-tags t
         :todo-keywords nil
         ;;:author nil
         :html-doctype "html5" ;; set doctype to html5
         :html-html5-fancy t
	 :html-table-caption-above nil
         :creator-info nil ;; don't insert creator's info
         :auto-postamble nil ;; Don't add any kind of html after the content
         :html-postamble nil ;; same thing
         :timestamp nil ;;
         :with-toc nil ;;
	 :section-numbers nil ;;
         :exclude-tags ("noexport" "todo")) ;; just in case we don't want to publish some part of the files
        ("blog-post" :components ("base-posts" "blog-static")) ;; meta identifier to publish everything at once
        ("blog-page" :components ("base-pages" "blog-static")) ;; meta identifier to publish everything at once

        ))

(require 'org-macro) ;; enable org-macro
;; this is a custom macro until I find out how to use defadvice
;; in order to put the <meta name="date"...> in the exported file
;; in the org file we would put {{{dt(2010-12-31 23:59)}}}
(defun apply-my-macros (backend)
  (org-macro-replace-all
   (list (cons "dt" "#+HTML_HEAD: <meta name=\"date\" content=\"$1\" />"))))
(add-hook 'org-export-before-processing-hook 'apply-my-macros)
(provide 'org-pelican)
