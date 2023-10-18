;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
                                        ;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
                                        ;(package! another-package
                                        ;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
                                        ;(package! this-package
                                        ;  :recipe (:host github :repo "username/repo"
                                        ;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
                                        ;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
                                        ;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
                                        ;(unpin! pinned-package)
;; ...or multiple packages
                                        ;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
                                        ;(unpin! t)

;; Aesthetic
(package! material-theme)
(package! spacemacs-theme :pin "1ec73d68b0f120f92538d9a329a3a46e32f74510")
(package! leuven-theme)
(package! poet-theme)

(package! marginalia)

;; Org packages
(package! org-bullets)
(package! ox-gfm)
(package! org-appear)
(package! org-fragtog)
(package! org-transclusion)

(unpin! org-roam)
(package! org-roam-ui)

;; Bibliography
;; When using org-roam via the `+roam` flag
;;(unpin! org-roam)
;;(package! org-roam-bibtex
;;  :recipe (:host github :repo "org-roam/org-roam-bibtex"))
;; When using bibtex-completion via the `biblio` module
;;(unpin! bibtex-completion helm-bibtex ivy-bibtex)
;;(package! org-ref)
(package! org-roam-bibtex
  :recipe (:host github :repo "org-roam/org-roam-bibtex"))

;; When using org-roam via the `+roam` flag
(unpin! org-roam)

;; When using bibtex-completion via the `biblio` module
(unpin! bibtex-completion helm-bibtex ivy-bibtex)

(package! org-ref)

;; Reveal presentations with org
(package! ox-reveal)

;; From tecosaur
;; Rotate (window management)
;;(package! rotate :pin "4e9ac3ff800880bd9b705794ef0f7c99d72900a6")

;; Info colors manual
(package! info-colors :pin "2e237c301ba62f0e0286a27c1abe48c4c8441143")

;; Org modern
(package! org-modern :pin "7d037569bc4a05f40262ea110c4cda05c69b5c52")

;; ORG HEADERS TREE
(package! org-ol-tree :recipe (:host github :repo "Townk/org-ol-tree"))

;; ORG-TRANSCLUSION
(package! org-transclusion :recipe (:host github :repo "nobiot/org-transclusion")
  :pin "cf51df7b87e0d32ba13ac5380557e81d9845d81b")

;; ORG-CITE-CSL-ACTIVATE
(package! org-cite-csl-activate :recipe (:host github :repo "andras-simonyi/org-cite-csl-activate") :pin "9e68d9204469c674f49a20bdf7ea85da4f4bf720")

;; org-roam-citar
(package! citar-org-roam)

;; Scihub connection
(package! scihub :recipe (:host github :repo "emacs-pe/scihub.el"))

;; Origami folds
;;(package! origami)

;; Mermaid diagrams
(package! mermaid-mode)
(package! ob-mermaid)
