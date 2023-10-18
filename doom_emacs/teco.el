;;; teco.el -*- lexical-binding: t; -*-
;; Config based on https://tecosaur.github.io/emacs-config/config.html

;; Better defaults

(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…"                ; Unicode ellispis are nicer than "...", and also save /precious/ space
      password-cache-expiry nil                   ; I can trust my computers ... can't I?
      ;; scroll-preserve-screen-position 'always     ; Don't have `point' jump around
      scroll-margin 2                             ; It's nice to maintain a little margin
      display-time-default-load-average nil)      ; I don't think I've ever found this useful

(display-time-mode 1)                             ; Enable time in the mode-line

(unless (string-match-p "^Power N/A" (battery))   ; On laptops...
  (display-battery-mode 1))                       ; it's nice to know how much power you have

(global-subword-mode 1)                           ; Iterate through CamelCase words

;; Frame sizing
(add-to-list 'default-frame-alist '(height . 24))
(add-to-list 'default-frame-alist '(width . 80))

;; Windows

;; Ask for buffer after splitting window
(setq evil-vsplit-window-right t
      evil-split-window-below t)
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (consult-buffer))

;; Change arrow-key variants of buffer management
(map! :map evil-window-map
      "SPC" #'rotate-layout
      ;; Navigation
      "<left>"     #'evil-window-left
      "<down>"     #'evil-window-down
      "<up>"       #'evil-window-up
      "<right>"    #'evil-window-right
      ;; Swapping windows
      "C-<left>"       #'+evil/window-move-left
      "C-<down>"       #'+evil/window-move-down
      "C-<up>"         #'+evil/window-move-up
      "C-<right>"      #'+evil/window-move-right)

;; Window title
(setq frame-title-format
      '(""
        (:eval
         (if (string-match-p (regexp-quote (or (bound-and-true-p org-roam-directory) "\u0000"))
                             (or buffer-file-name ""))
             (replace-regexp-in-string
              ".*/[0-9]*-?" "☰ "
              (subst-char-in-string ?_ ?\s buffer-file-name))
           "%b"))
        (:eval
         (when-let ((project-name (and (featurep 'projectile) (projectile-project-name))))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s") project-name))))))

;; Which key
(setq which-key-idle-delay 0.5) ;; I need the help, I really do

;; Very large files
;; (use-package! vlf-setup
;;   :defer-incrementally vlf-tune vlf-base vlf-write
;;   vlf-search vlf-occur vlf-follow vlf-ediff vlf
;;   :commands vlf vlf-mode
;;   :init
;;   <<vlf-largefile-prompt>>
;;   :config
;;   (advice-remove 'abort-if-file-too-large #'ad-Advice-abort-if-file-too-large)
;;   <<vlf-linenum-offset>>
;;   <<vlf-search-chunking>>)

;; (defadvice! +files--ask-about-large-file-vlf (size op-type filename offer-raw)
;;   "Like `files--ask-user-about-large-file', but with support for `vlf'."
;;   :override #'files--ask-user-about-large-file
;;   (if (eq vlf-application 'dont-ask)
;;       (progn (vlf filename) (error ""))
;;     (let ((prompt (format "File %s is large (%s), really %s?"
;;                           (file-name-nondirectory filename)
;;                           (funcall byte-count-to-string-function size) op-type)))
;;       (if (not offer-raw)
;;           (if (y-or-n-p prompt) nil 'abort)
;;         (let ((choice
;;                (car
;;                 (read-multiple-choice
;;                  prompt '((?y "yes")
;;                           (?n "no")
;;                           (?l "literally")
;;                           (?v "vlf"))
;;                  (files--ask-user-about-large-file-help-text
;;                   op-type (funcall byte-count-to-string-function size))))))
;;           (cond ((eq choice ?y) nil)
;;                 ((eq choice ?l) 'raw)
;;                 ((eq choice ?v)
;;                  (vlf filename)
;;                  (error ""))
;;                 (t 'abort)))))))

;; (defvar-local +vlf-cumulative-linenum '((0 . 0))
;;   "An alist keeping track of the cumulative line number.")

;; (defun +vlf-update-linum ()
;;   "Update the line number offset."
;;   (let ((linenum-offset (alist-get vlf-start-pos +vlf-cumulative-linenum)))
;;     (setq display-line-numbers-offset (or linenum-offset 0))
;;     (when (and linenum-offset (not (assq vlf-end-pos +vlf-cumulative-linenum)))
;;       (push (cons vlf-end-pos (+ linenum-offset
;;                                  (count-lines (point-min) (point-max))))
;;             +vlf-cumulative-linenum))))

;; (add-hook 'vlf-after-chunk-update-hook #'+vlf-update-linum)

;; ;; Since this only works with absolute line numbers, let's make sure we use them.
;; (add-hook! 'vlf-mode-hook (setq-local display-line-numbers t))

;; (defun +vlf-next-chunk-or-start ()
;;   (if (= vlf-file-size vlf-end-pos)
;;       (vlf-jump-to-chunk 1)
;;     (vlf-next-batch 1))
;;   (goto-char (point-min)))

;; (defun +vlf-last-chunk-or-end ()
;;   (if (= 0 vlf-start-pos)
;;       (vlf-end-of-file)
;;     (vlf-prev-batch 1))
;;   (goto-char (point-max)))

;; (defun +vlf-isearch-wrap ()
;;   (if isearch-forward
;;       (+vlf-next-chunk-or-start)
;;     (+vlf-last-chunk-or-end)))

;; (add-hook! 'vlf-mode-hook (setq-local isearch-wrap-function #'+vlf-isearch-wrap))

;; ;; Company

(after! company
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2)
  (setq company-show-numbers t)
  (add-hook 'evil-normal-state-entry-hook #'company-abort)) ;; make aborting less annoying.

(setq-default history-length 1000)
(setq-default prescient-history-length 1000)

;; ESS
(set-company-backend! 'ess-r-mode '(company-R-args company-R-objects company-dabbrev-code :separate))

;; Visuals
(use-package! info-colors
  :commands (info-colors-fontify-node))

(add-hook 'Info-selection-hook 'info-colors-fontify-node)

;; Centaur tabs
(after! centaur-tabs
  (centaur-tabs-mode -1)
  (setq centaur-tabs-height 36
        centaur-tabs-set-icons t
        centaur-tabs-modified-marker "o"
        centaur-tabs-close-button "×"
        centaur-tabs-set-bar 'above
        centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-change-fonts "P22 Underground Book" 160))

;; Writeroom
(defvar +zen-serif-p t
  "Whether to use a serifed font with `mixed-pitch-mode'.")
(defvar +zen-org-starhide t
  "The value `org-modern-hide-stars' is set to.")

(after! writeroom-mode
  (defvar-local +zen--original-org-indent-mode-p nil)
  (defvar-local +zen--original-mixed-pitch-mode-p nil)
  (defun +zen-enable-mixed-pitch-mode-h ()
    "Enable `mixed-pitch-mode' when in `+zen-mixed-pitch-modes'."
    (when (apply #'derived-mode-p +zen-mixed-pitch-modes)
      (if writeroom-mode
          (progn
            (setq +zen--original-mixed-pitch-mode-p mixed-pitch-mode)
            (funcall (if +zen-serif-p #'mixed-pitch-serif-mode #'mixed-pitch-mode) 1))
        (funcall #'mixed-pitch-mode (if +zen--original-mixed-pitch-mode-p 1 -1)))))
  (defun +zen-prose-org-h ()
    "Reformat the current Org buffer appearance for prose."
    (when (eq major-mode 'org-mode)
      (setq display-line-numbers nil
            visual-fill-column-width 60
            org-adapt-indentation nil)
      (when (featurep 'org-modern)
        (setq-local org-modern-star '("🙘" "🙙" "🙚" "🙛")
                    ;; org-modern-star '("🙐" "🙑" "🙒" "🙓" "🙔" "🙕" "🙖" "🙗")
                    org-modern-hide-stars +zen-org-starhide)
        (org-modern-mode -1)
        (org-modern-mode 1))
      (setq
       +zen--original-org-indent-mode-p org-indent-mode)
      (org-indent-mode -1)))
  (defun +zen-nonprose-org-h ()
    "Reverse the effect of `+zen-prose-org'."
    (when (eq major-mode 'org-mode)
      (when (bound-and-true-p org-modern-mode)
        (org-modern-mode -1)
        (org-modern-mode 1))
      (when +zen--original-org-indent-mode-p (org-indent-mode 1))))
  (pushnew! writeroom--local-variables
            'display-line-numbers
            'visual-fill-column-width
            'org-adapt-indentation
            'org-modern-mode
            'org-modern-star
            'org-modern-hide-stars)
  (add-hook 'writeroom-mode-enable-hook #'+zen-prose-org-h)
  (add-hook 'writeroom-mode-disable-hook #'+zen-nonprose-org-h))

;; ORG MODE
(use-package! org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
        org-modern-table-vertical 1
        org-modern-table-horizontal 0.2
        org-modern-list '((43 . "➤")
                          (45 . "–")
                          (42 . "•"))
        org-modern-todo-faces
        '(("TODO" :inverse-video t :inherit org-todo)
          ("PROJ" :inverse-video t :inherit +org-todo-project)
          ("STRT" :inverse-video t :inherit +org-todo-active)
          ("[-]"  :inverse-video t :inherit +org-todo-active)
          ("HOLD" :inverse-video t :inherit +org-todo-onhold)
          ("WAIT" :inverse-video t :inherit +org-todo-onhold)
          ("[?]"  :inverse-video t :inherit +org-todo-onhold)
          ("KILL" :inverse-video t :inherit +org-todo-cancel)
          ("NO"   :inverse-video t :inherit +org-todo-cancel))
        org-modern-footnote
        (cons nil (cadr org-script-display))
        org-modern-block-fringe nil
        org-modern-block-name
        '((t . t)
          ("src" "»" "«")
          ("example" "»–" "–«")
          ("quote" "❝" "❞")
          ("export" "⏩" "⏪"))
        org-modern-progress nil
        org-modern-priority nil
        org-modern-horizontal-rule (make-string 36 ?─)
        org-modern-keyword
        '((t . t)
          ("title" . "𝙏")
          ("subtitle" . "𝙩")
          ("author" . "𝘼")
          ("email" . #("" 0 1 (display (raise -0.14))))
          ("date" . "𝘿")
          ("property" . "☸")
          ("options" . "⌥")
          ("startup" . "⏻")
          ("macro" . "𝓜")
          ("bind" . #("" 0 1 (display (raise -0.1))))
          ("bibliography" . "")
          ("print_bibliography" . #("" 0 1 (display (raise -0.1))))
          ("cite_export" . "⮭")
          ("print_glossary" . #("ᴬᶻ" 0 1 (display (raise -0.1))))
          ("glossary_sources" . #("" 0 1 (display (raise -0.14))))
          ("include" . "⇤")
          ("setupfile" . "⇚")
          ("html_head" . "🅷")
          ("html" . "🅗")
          ("latex_class" . "🄻")
          ("latex_class_options" . #("🄻" 1 2 (display (raise -0.14))))
          ("latex_header" . "🅻")
          ("latex_header_extra" . "🅻⁺")
          ("latex" . "🅛")
          ("beamer_theme" . "🄱")
          ("beamer_color_theme" . #("🄱" 1 2 (display (raise -0.12))))
          ("beamer_font_theme" . "🄱𝐀")
          ("beamer_header" . "🅱")
          ("beamer" . "🅑")
          ("attr_latex" . "🄛")
          ("attr_html" . "🄗")
          ("attr_org" . "⒪")
          ("call" . #("" 0 1 (display (raise -0.15))))
          ("name" . "⁍")
          ("header" . "›")
          ("caption" . "☰")
          ("results" . "🠶")))
  (custom-set-faces! '(org-modern-statistics :inherit org-checkbox-statistics-todo)))

(after! spell-fu
  (cl-pushnew 'org-modern-tag (alist-get 'org-mode +spell-excluded-faces-alist)))

;; org-appear
(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks nil)
  ;; for proper first-time setup, `org-appear--set-elements'
  ;; needs to be run after other hooks have acted.
  (run-at-time nil nil #'org-appear--set-elements))

;; ORG transclusion
(use-package! org-transclusion
  :commands org-transclusion-mode
  :init
  (map! :after org :map org-mode-map
        "<f12>" #'org-transclusion-mode))

;; ORG IMPORT
(use-package! org-pandoc-import :after org)

;; ORG MUSIC
(use-package! org-music
  :after org
  :config
  (setq org-music-mpris-player "Lollypop"
        org-music-track-search-method 'beets
        org-music-beets-db "~/Music/library.db"))

;; Selectric
(use-package! selectic-mode
  :commands selectic-mode)
