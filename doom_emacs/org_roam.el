;; ORG roam config

(setq org-roam-v2-ack t)

(use-package! org-roam
  :after org
  :config
  (setq org-roam-v2-ack t)
  (setq org-roam-mode-sections
        (list #'org-roam-backlinks-insert-section
              #'org-roam-reflinks-insert-section
              #'org-roam-unlinked-references-insert-section))
  (org-roam-setup))

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(after! org-roam
    (setq org-roam-capture-templates
          `(("s" "standard" plain "%?"
     :if-new
     (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
      "#+title: ${title}\n#+filetags: \n\n ")
     :unnarrowed t)
        ("d" "definition" plain
         "%?"
         :if-new
         (file+head "${slug}.org" "#+title: ${title}\n#+filetags: definition \n\n* Definition\n\n\n* Examples\n")
         :unnarrowed t)
        ("r" "ref" plain "%?"
           :if-new
           (file+head "${citekey}.org"
           "#+title: ${slug}: ${title}\n
\n#+filetags: reference ${keywords} \n
\n* ${title}\n\n
\n* Summary
\n\n\n* Rough note space\n")
           :unnarrowed t)
          ("p" "person" plain "%?"
           :if-new
           (file+head "${slug}.org" "%^{relation|some guy|family|friend|colleague}p %^{birthday}p %^{address}p
#+title:${slug}\n#+filetags: :person: \n"
                      :unnarrowed t)))))

(use-package! org-ref
    ;:after org-roam
    :config
    (setq
         org-ref-completion-library 'org-ref-ivy-cite
         org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
         org-ref-default-bibliography (list "/Documents/biblio/main.bib")
         org-ref-bibliography-notes "/Documents/org/roam/bibnotes.org"
         org-ref-note-title-format "* %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
         org-ref-notes-directory "/Documents/org/roam/"
         org-ref-notes-function 'orb-edit-notes
    ))

(after! org-ref
(setq
 bibtex-completion-notes-path "/Documents/org/roam/"
 bibtex-completion-bibliography "/Documents/main.bib"
 bibtex-completion-pdf-field "file"
 bibtex-completion-notes-template-multiple-files
 (concat
  "#+TITLE: ${title}\n"
  "#+ROAM_KEY: cite:${=key=}\n"
  "* TODO Notes\n"
  ":PROPERTIES:\n"
  ":Custom_ID: ${=key=}\n"
  ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
  ":AUTHOR: ${author-abbrev}\n"
  ":JOURNAL: ${journaltitle}\n"
  ":DATE: ${date}\n"
  ":YEAR: ${year}\n"
  ":DOI: ${doi}\n"
  ":URL: ${url}\n"
  ":END:\n\n"
  )
 )
)

(use-package! org-roam-bibtex
  :after org-roam
  :hook (org-mode . org-roam-bibtex-mode)
  :config
  (require 'org-ref)
  (setq orb-preformat-keywords
   '("citekey" "title" "url" "file" "author-or-editor" "keywords" "pdf" "doi" "author" "tags" "year" "author-bbrev")))
;)
