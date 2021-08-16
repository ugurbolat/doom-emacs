(setq visual-fill-column-width 150)
(setq visual-fill-column-center-text t)
(global-visual-fill-column-mode 1)


(defun ub/org-mode-visual-fill ()
  (setq visual-fill-column-width 150
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

;; NOTE doesn't work inside after! org as :hook
(add-hook! org-mode ub/org-mode-visual-fill)

(defun ub/prog-mode-visual-fill ()
  (setq visual-fill-column-width 150
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(add-hook! prog-mode ub/prog-mode-visual-fill)

(after! org
  :config
  ;; enable inline image
  (setq org-startup-with-inline-images t)
  ;; enable org-indent mode which is a soft indent that doesn't edit the file
  (setq org-startup-indented t)
  ;; org latex
  ;;(setq org-startup-with-latex-preview t)
  (setq org-startup-folded 'fold)
  (setq org-checkbox-hierarchical-statistics nil)
  (setq org-tags-column -77)
  (setq pdf-annot-default-annotation-properties
        '((t (label . "Ugur Bolat"))
          (text (color . "gold") (icon . "Note"))
          (highlight (color . "yellow"))
          (underline (color . "blue"))
          (squiggly (color . "orange"))
          (strike-out (color . "red"))))
                                        ;(setq org-pdftools-link-prefix "pdf")
  (setq org-enforce-todo-dependencies nil)
  (setq org-log-state-notes-into-drawer nil)
  )

(global-set-key "\C-ca" 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c C-l") 'org-insert-link)
;; (global-set-key "\C-cc" 'org-capture)

(setq org-export-backends (quote (ascii html icalendar latex md odt taskjuggler)))
(setq org-export-with-drawers (quote (selected)))

(use-package! org-noter
  :after (:any org pdf-view)
  :config
  ;; helpful in EXWM, where there are no frames
  ;; (customize-set-variable 'org-noter-always-create-frame nil)
  ;; (customize-set-variable 'org-noter-notes-window-behavior '(start))
  ;;(customize-set-variable 'org-noter-notes-window-location 'horizontal-split)
  (setq org-noter-always-create-frame nil
        org-noter-notes-window-behavior '(start)
        org-noter-notes-window-location 'other-frame
        org-noter-notes-search-path `(,ub/zk-phd-dir)
        org-noter-auto-save-last-location t
        org-noter-default-notes-file-names '("tmp_notes.org"))

  ;; This works for assigning PDF paths, but then breaks when trying to find the tpath later.
  ;; (defadvice! better-org-noter--get-or-read-document-property (orig-fn &rest args)
  ;;   :around 'org-noter--get-or-read-document-property
  ;;   (let ((default-directory (if (boundp 'my/noter-default-directory)
  ;;                                my/noter-default-directory
  ;;                              default-directory) ))
  ;;     (apply orig-fn args)))
  )

(when (eq system-type 'windows-nt)
  (add-to-list 'exec-path ub/windows-sql-bin-path)
  (setq mouse-wheel-progressive-speed nil))

;; (defun org-roam-server-open ()
;;   "Ensure the server is active, then open the roam graph."
;;   (interactive)
;;   (smartparens-global-mode -1)
;;   (org-roam-server-mode 1)
;;   (if (eq system-type 'windows-nt)
;;       (browse-url (format "http://localhost:%d" org-roam-server-port))
;;     (browse-url-xdg-open (format "http://localhost:%d" org-roam-server-port)))
;;   (smartparens-global-mode 1))

;; (use-package! org-roam
;;   :hook
;;   (after-init . org-roam-mode)
;;   :custom
;;   (org-roam-directory ub/zk-refile-dir)
;;   (org-roam-db-location (expand-file-name ".refile-roam.db" ub/zk-refile-dir))
;;   :bind (:map org-roam-mode-map
;;          (("C-c u z n" . org-roam-capture)
;;           ;; ("C-c u z f" . org-roam-find-file)
;;           ;; ("C-c u z g" . org-roam-server-open)
;;           ("C-c u z r r" . ub/refile-zk-to-refile)
;;           ("C-c u z r p" . ub/refile-zk-to-private)
;;           ("C-c u z r s" . ub/refile-zk-to-shared)
;;           ("C-c u z r h" . ub/refile-zk-to-phd)
;;           )
;;          :map org-mode-map
;;          (("C-c u z i" . org-roam-insert))
;;          (("C-c u z I" . org-roam-insert-immediate)))
;;   )

(use-package! org-roam
      :custom
      (org-roam-directory ub/zk-refile-dir)
      (org-roam-db-location (expand-file-name ".refile-roam.db" ub/zk-refile-dir))
      :bind (("C-c n l" . org-roam-buffer-toggle)
             ("C-c n f" . org-roam-node-find)
             ("C-c n g" . org-roam-graph)
             ("C-c n i" . org-roam-node-insert)
             ("C-c n c" . org-roam-capture)
             ;; Dailies
             ("C-c n j" . org-roam-dailies-capture-today))
      :config
      (org-roam-setup)
      ;; If using org-roam-protocol
      (require 'org-roam-protocol))

(setq org-roam-v2-ack t)

;; NOTE Ref: https://gist.github.com/progfolio/2871f12099be3913a1c61a3c3062f4fd
(require 'doct)
(eval-when-compile (require 'subr-x))

(defun doct-org-roam-convert (groups)
  "Convert GROUPS of templates to `org-roam' compatible templates."
  (setq doct-templates
        (mapcar (lambda (template)
                  (if-let* ((props (nthcdr (if (= (length template) 4) 2 5) template))
                            (org-roam-props (plist-get (plist-get props :doct) :org-roam)))
                      `(,@template ,@org-roam-props)
                    template))
                (doct-flatten-lists-in groups))))

(defun doct-org-roam (declarations)
  "Convert DECLARATIONS to `org-roam-capture-templates'.
DECLARATIONS must be of the same form that `doct' expects with
one addition: the :org-roam keyword.
The :org-roam keyword's value must be a plist mapping `org-roam''s
template syntax extensions (e.g. :file-name :head) to their appropriate values.
Note this does validate the :org-roam plist's values or keywords."

  ;;TODO: we should preserve doct-after-conversion-functions
  ;;in case user already has other functions set.
  (let ((doct-after-conversion-functions (append '(doct-org-roam-convert)
                                                 doct-after-conversion-functions)))
    (doct declarations)))


(provide 'doct-org-roam)


(defun ub/roam-ref-capture-hook ()
  (interactive)
  (org-id-get-create)
  ;; NOTE doesnt work :/ do it manually for a now
  ;;(org-download-clipboard)
  )

;; (setq org-roam-capture-ref-templates
;;       (doct-org-roam '("Visual DB"
;;                        :keys "v"
;;                        :type plain
;;                        :function org-roam--capture-get-point
;;                        :before-finalize ub/roam-ref-capture-hook
;;                        :immediate-finish t
;;                        :jump-to-captured t
;;                        :template
;;                        ("\n\n* N@DONE ${title} %?%i"
;;                         ":PROPERTIES:"
;;                         ":REF-LINK: %a"
;;                         ":CREATED: %U"
;;                         ":END:")
;;                        :org-roam ( :file-name "%(concat ub/vdb-dir (format-time-string \"/%Y%m%d_${slug}\" (current-time) t))"
;;                                    :head "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n#+ROAM_TAGS: visualdb\n\n")
;;                        :unnarrowed t)))

(setq org-roam-capture-ref-templates
      '(
        ("r" "Refile" plain "%?"
         :if-new (file+head "%<%Y%m%d>-${slug}.org"
                            "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n#+FILETAGS: refile\n\n")
         :unnarrowed t)
        ("v" "Visual DB" plain "%?"
         :if-new (file+head
                  "%(concat ub/vdb-dir (format-time-string \"/%Y%m%d_${slug}.org\" (current-time) t))"
                            "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n#+FILETAGS: visualdb\n\n")
         :unnarrowed t
         :immediate-finish t
         :jump-to-captured t)
        ))

(setq org-roam-dailies-directory "refile/")
;; (setq org-roam-dailies-capture-templates
;;       '(
;;         ("r" "Refile" plain (function org-roam--capture-get-point)
;;          "\n\n* NOTE %?%i"
;;          :file-name "%(format-time-string \"refile/%Y%m%d_${slug}\" (current-time) t)"
;;          :head "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n#+ROAM_KEY: ${ref}\n#+ROAM_TAGS: refile\n\n"
;;          :immediate-finish t
;;          :jump-to-captured t
;;          :unnarrowed t)
;;         ))
;; NOTE id create and title doesn't work
;; (setq org-roam-dailies-capture-templates
;;       (doct-org-roam '("default"
;;                        :keys "d"
;;                        :type plain
;;                        :function org-roam--capture-get-point
;;                        :before-finalize (lambda () (org-id-get-create))
;;                        :immediate-finish t
;;                        :jump-to-captured t
;;                        :template "\n\n* NOTE %?%i"
;;                        :org-roam ( :file-name "%<%Y%m%d>-${slug}"
;;                                    :head "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n#+ROAM_TAGS: refile\n\n")
;;                        :unnarrowed t)))



(setq org-roam-capture-templates
      '(
        ("r" "Refile" plain "%?"
         :if-new (file+head "%(concat ub/zk-refile-dir (format-time-string \"/%Y%m%d-${slug}.org\" (current-time) t))"
                            "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n#+FILETAGS: refile\n\n")
         :unnarrowed t)


        ("z" "ZK")

        ("zp" "private" plain "%?"
         :if-new (file+head "%(concat ub/zk-private-dir (format-time-string \"/%Y%m%d_${slug}.org\" (current-time) t))"
                            "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n#+FILETAGS: zk private\n\n")
         :unnarrowed t)

        ("zs" "shared" plain "%?"
         :if-new (file+head "%(concat ub/zk-shared-dir (format-time-string \"/%Y%m%d_${slug}.org\" (current-time) t))"
                            "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n#+FILETAGS: zk shared\n\n")
         :unnarrowed t)

                ("zh" "phd" plain "%?"
         :if-new (file+head "%(concat ub/zk-phd-dir (format-time-string \"/%Y%m%d_${slug}.org\" (current-time) t))"
                            "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n#+FILETAGS: zk phd\n\n")
         :unnarrowed t)

                ("j" "Project" plain
                 "\n\n* PROJECT ${title} %?%i   :project_title:\n:PROPERTIES:\n:CREATED: %U\n"
;;                          ":END:"
         :if-new (file+head "%(concat ub/gtd-root-dir \"/\" (ido-completing-read \"Select directory: \" ub/gtd-projects-dir-name-list) \".projects\" \"/pj_${slug}.org\")"
                            "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n#+FILETAGS: zk phd\n\n")
         :unnarrowed t)

        ))

;; (setq org-roam-capture-templates
;;       (doct-org-roam `(("Refile"
;;                         :keys "r"
;;                         :warn (:not unbound)
;;                         :type plain
;;                         :function org-roam-capture--get-point
;;                         :before-finalize (lambda () (org-id-get-create))
;;                         :template
;;                         ("\n\n* N@NEXT ${title} %?%i"
;;                          ":PROPERTIES:"
;;                          ":CREATED: %U"
;;                          ":END:")
;;                         :if-new
;;                                    (:file+head "%(concat ub/zk-refile-dir (format-time-string \"/%Y%m%d_${slug}\" (current-time) t))"
;;                                     "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n#+ROAM_TAGS: refile\n\n"
;;                                    ))
;;                        ("Literature"
;;                         :keys "l"
;;                         :warn (:not unbound)
;;                         :type plain
;;                         :function org-roam-capture--get-point
;;                         :before-finalize (lambda () (org-id-get-create))
;;                         :template
;;                         ("\n\n* N@NEXT ${title} %?%i   :literature:"
;;                          ":PROPERTIES:"
;;                          ":CREATED: %U"
;;                          ":END:"
;;                          "\n"
;;                          "** The Gist\n"
;;                          "** Keypoints\n"
;;                          "** Thoughts and Questions\n"
;;                          "** References\n")
;;                         :org-roam (:file-name "%(concat ub/zk-refile-dir (format-time-string \"/%Y%m%d_${slug}\" (current-time) t))"
;;                                    :head "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n#+ROAM_TAGS: literature\n\n"
;;                                    :unnarrowed t
;;                                    ))
;;                        ("Contemplation"
;;                         :keys "c"
;;                         :warn (:not unbound)
;;                         :type plain
;;                         :function org-roam-capture--get-point
;;                         :before-finalize (lambda () (org-id-get-create))
;;                         :template
;;                         ("\n\n* N@NEXT ${title} %?%i   :contemplation:"
;;                          ":PROPERTIES:"
;;                          ":CREATED: %U"
;;                          ":END:")
;;                         :org-roam (:file-name "%(concat ub/zk-refile-dir (format-time-string \"/%Y%m%d_${slug}\" (current-time) t))"
;;                                    :head "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n#+ROAM_TAGS: contemplation\n\n"
;;                                    :unnarrowed t
;;                                    ))
;;                        ("zk"
;;                         :keys "z"
;;                         :warn (:not unbound)
;;                         :type plain
;;                         :function org-roam-capture--get-point
;;                         :before-finalize (lambda () (org-id-get-create))
;;                         :template
;;                         ("\n\n* N@NEXT ${title} %?%i  :zk:"
;;                          ":PROPERTIES:"
;;                          ":CREATED: %U"
;;                          ":END:")
;;                         :children (
;;                                    ("private" :keys "p"
;;                                     :org-roam (:file-name "%(concat ub/zk-private-dir (format-time-string \"/%Y%m%d_${slug}\" (current-time) t))"
;;                                                :head "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n#+ROAM_TAGS: zk\n\n"
;;                                                :unnarrowed t))
;;                                    ("shared" :keys "s"
;;                                     :org-roam (:file-name "%(concat ub/zk-shared-dir (format-time-string \"/%Y%m%d_${slug}\" (current-time) t))"
;;                                                :head "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n#+ROAM_TAGS: zk\n\n"
;;                                                :unnarrowed t))
;;                                    ("phd" :keys "h"
;;                                     :org-roam (:file-name "%(concat ub/zk-phd-dir (format-time-string \"/%Y%m%d_${slug}\" (current-time) t))"
;;                                                :head "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n#+ROAM_TAGS: zk\n\n"
;;                                                :unnarrowed t))
;;                                    )
;;                         )
;;                        ("VisualDB"
;;                         :keys "v"
;;                         :warn (:not unbound)
;;                         :type plain
;;                         :function org-roam-capture--get-point
;;                         :before-finalize (lambda () (org-id-get-create))
;;                         ;;:immediate-finish t
;;                         :template
;;                         ("\n\n* N@DONE ${title} %?%i   :visualdb:"
;;                          ":PROPERTIES:"
;;                          ":CREATED: %U"
;;                          ":END:")
;;                         :org-roam (:file-name "%(concat ub/vdb-dir (format-time-string \"/%Y%m%d_${slug}\" (current-time) t))"
;;                                    :head "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n#+ROAM_TAGS: visualdb\n\n"
;;                                    :unnarrowed t
;;                                    ))
;;                        ("Project"
;;                         :keys "p"
;;                         :warn (:not unbound)
;;                         :type plain
;;                         :function org-roam-capture--get-point
;;                         :before-finalize (lambda () (org-id-get-create))
;;                         :template
;;                         ("\n\n* PROJECT ${title} %?%i   :project_title:"
;;                          ":PROPERTIES:"
;;                          ":CREATED: %U"
;;                          ":END:")
;;                         :org-roam (:file-name "%(concat ub/gtd-root-dir \"/\" (ido-completing-read \"Select directory: \" ub/gtd-projects-dir-name-list) \".projects\" \"/pj_${slug}\")"
;;                                    :head "#+TITLE: ${slug}\n#+DATE: %<%Y-%m-%d>\n#+FILETAGS: project_file\n#+ROAM_TAGS: project\n#+ARCHIVE: archive/${slug}_archive.org::\n"
;;                                    :unnarrowed t)
;;                         )
;;                        )))

(setq org-roam-capture-immediate-template
      '("d" "default" plain (function org-roam--capture-get-point)
        "\n\n* NOTE ${title} %?%i\n:PROPERTIES:\n:ID:       %(shell-command-to-string \"uuidgen\"):CREATED: %U\n:END:\n"
        :file-name "%(format-time-string \"refile/%Y%m%d_${slug}\" (current-time) t)"
        :head "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n#+ROAM_TAGS: refile\n\n"
        :immediate-finish t
        :unnarrowed t))

;; NOTE doct doesn't work with immediate-template
;; (setq  org-roam-capture-immediate-template
;;       (doct-org-roam `("default"
;;                        :keys "d"
;;                        :type plain
;;                        :function org-roam--capture-get-point
;;                        :before-finalize (lambda () (org-id-get-create))
;;                        :immediate-finish t
;;                        :template
;;                        ("\n\n* NOTE ${title} %?%i   :contemplation:"
;;                         ":PROPERTIES:"
;;                         ":CREATED: %U"
;;                         ":END:")
;;                        :org-roam (:file-name "%(format-time-string \"refile/%Y%m%d_${slug}\" (current-time) t)"
;;                                   :head "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n#+ROAM_TAGS: refile\n\n")
;;                        :unnarrowed t)))
;; (setq org-roam-capture-immediate-template
;;       (doct-org-roam '("default"
;;                        :keys "d"
;;                        :type plain
;;                        :function org-roam--capture-get-point
;;                        :immediate-finish t
;;                        :template "%?"
;;                        :org-roam ( :file-name "%<%Y%m%d%H%M%S>-${slug}"
;;                                    :head "#+title: ${title}\n")
;;                        :unnarrowed t)))

;; it doesn't seem to sort based on heading ? :/
(defun ub/zk-refile ()
  (interactive)
  (org-ql-search (lambda () ub/zk-refile-files-list)
    '(todo)
    :sort '(date priority todo))
  ;;:super-groups
  )
(map! "C-c u z q" #'ub/zk-refile)

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;    :hook
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
    :hook (after-init . org-roam-ui-mode)
    :bind (("C-c n u" . org-roam-ui-mode))
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(after! org-ref
  (setq
   bibtex-completion-notes-path ub/zk-phd-dir
   bibtex-completion-bibliography ub/bib-files-list  ;; NOTE for a now
   bibtex-completion-pdf-field "file"
   bibtex-completion-notes-template-multiple-files
   (concat
    "#+TITLE: ${title}\n"
    "#+ROAM_KEY: cite:${=key=}\n"
    "* NOTE ${title}\n"
    ":CREATED: %U\n"
    ":PROPERTIES:\n"
    ":Custom_ID: ${=key=}\n"
    ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
    ":AUTHOR: ${author-abbrev}\n"
    ":JOURNAL: ${journaltitle}\n"
    ":DATE: ${date}\n"
    ":YEAR: ${year}\n"
    ":DOI: ${doi}\n"
    ":URL: ${url}\n"
    ":END:\n\n")))
(map! "C-c u b" #'helm-bibtex)

;; https://github.com/jkitchin/org-ref/issues/184
(defun ub/org-ref-open-pdf-at-point ()
  "Open the pdf for bibtex key under point if it exists."
  (interactive)
  (let* ((results (org-ref-get-bibtex-key-and-file))
         (key (car results))
         (pdf-file (car (bibtex-completion-find-pdf key))))
    (if (file-exists-p pdf-file)
        (funcall bibtex-completion-pdf-open-function pdf-file)
      (message "No PDF found for %s" key))))

(use-package! org-ref
  :config
  (setq
   org-ref-completion-library 'org-ref-ivy-cite
   org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
   org-ref-default-bibliography ub/bib-files-list
   org-ref-bibliography-notes (concat ub/zk-refile-dir "/bibnotes.org")
   org-ref-note-title-format "* NOTE %t\n:PROPERTIES:\n:CREATED: %U\n:Custom_ID: %k\n:NOTER_DOCUMENT: %F\n:ROAM_KEY: cite:%k\n:AUTHOR: %9a\n:JOURNAL: %j\n:YEAR: %y\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
   org-ref-notes-directory ub/zk-phd-dir
   org-ref-notes-function 'orb-edit-notes
   org-ref-open-pdf-function 'ub/org-ref-open-pdf-at-point
   ))

(setq ub/roam-bibtex-note-paper-template (concat "#+TITLE: ${title}\n"
                                                 "#+DATE: %<%Y-%m-%d>\n"
                                                 "#+ROAM_TAGS: literature paper\n"
                                                 "#+OPTIONS: toc:nil\n"
                                                 "#+ROAM_KEY: ${ref}\n\n"
                                                 "- tags ::\n\n"
                                                 "* N@NEXT ${title}\n"
                                                 ":PROPERTIES:\n"
                                                 ":CREATED: %U\n"
                                                 ":Custom_ID: ${citekey}\n"
                                                 ":URL: ${url}\n"
                                                 ":AUTHOR: ${author-or-editor}\n"
                                                 ":DOI: ${doi}\n"
                                                 ":DATE: ${date}\n"
                                                 ":YEAR: ${year}\n"
                                                 ":JOURNAL: ${journaltitle}\n"
                                                 ":NOTER_DOCUMENT: %(orb-process-file-field \"${citekey}\")\n"
                                                 ":NOTER_PAGE: \n"
                                                 ":END:\n\n"
                                                 "** The Gist\n%?\n"
                                                 "** Keypoints\n\n"
                                                 "** Thoughts and Questions\n\n"
                                                 "** References\n"
                                                 ))
(setq ub/roam-bibtex-note-media-template (concat "#+TITLE: ${title}\n"
                                                 "#+DATE: %<%Y-%m-%d>\n"
                                                 "#+ROAM_TAGS: literature media\n"
                                                 "#+OPTIONS: toc:nil\n"
                                                 "#+ROAM_KEY: ${ref}\n\n"
                                                 "- tags ::\n\n"
                                                 "* N@NEXT ${title}\n"
                                                 ":PROPERTIES:\n"
                                                 ":CREATED: %U\n"
                                                 ":Custom_ID: ${citekey}\n"
                                                 ":URL: [[video:${url}#0:00:00][${title}]]\n"
                                                 ":DATE: ${date}\n"
                                                 ":YEAR: ${year}\n"
                                                 ":END:\n\n"
                                                 "** The Gist\n%?\n"
                                                 "** Keypoints\n\n"
                                                 "** Thoughts and Questions\n\n"
                                                 "** References\n"
                                                 ))


(use-package org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (setq orb-preformat-keywords '("citekey" "title" "url" "file" "author-or-editor" "doi" "date" "year" "journaltitle")) ; NOTE list element order has to match with capture template order :/
  (setq orb-templates
        `(
          ;; ("r" "ref" plain (function org-roam-capture--get-point) ""
          ;;  :file-name "${citekey}"
          ;;  :head "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n"
          ;;  :unnarrowed t)
          ("p" "paper" plain (function org-roam-capture--get-point) ""
           :file-name  "%(concat ub/zk-refile-dir (format-time-string \"/%Y%m%d_${slug}\" (current-time) t))"
           :head ,ub/roam-bibtex-note-paper-template
           :unnarrowed t)
          ("m" "media" plain (function org-roam-capture--get-point) ""
           :file-name "%(concat ub/zk-refile-dir (format-time-string \"/%Y%m%d_${slug}\" (current-time) t))"
           :head ,ub/roam-bibtex-note-media-template
           :unnarrowed t)
          )))

(setq org-now-location `(,ub/now-file))
(setq org-now-no-other-window nil)
(add-hook 'org-now-hook 'visual-line-mode)

;; Disables the effect of openning extra window during org-capture
;; NOTE: in this way, it doesn't require to fork so if you have the
;; problem, refer to the local forked folder and see the change related to
;; find-file-other-window and find-file-noseleect for org-journal-find-file
(custom-set-variables '(org-journal-find-file 'find-file-noselect))

;; (setq org-journal-enable-encryption t) ; BUG doesn't encrypt automatically
(setq org-journal-prefix-key "C-c j")
(setq org-journal-dir ub/journal-dir)
(setq org-journal-date-format "%A %Y-%m-%d")
(setq org-journal-file-format "%Y_%U.org")
(setq org-journal-file-type 'weekly)
(setq org-journal-carryover-items "") ;; disable carry over todo items
(setq org-journal-start-on-weekday 1)
(setq org-journal-date-prefix "* ")

;; Close journal on exit
(defun org-journal-save-entry-and-exit ()
  "Simple convenience function.
  Saves the buffer of the current day's entry and kills the window
  Similar to org-capture like behavior"
  (interactive)
  ;;(save-buffer)
  (+workspace/close-window-or-workspace))

;; Close journal on exit
(defun org-journal-exit-and-agenda-redo ()
  "Simple convenience function.
  Saves the buffer of the current day's entry and kills the window
  Similar to org-capture like behavior"
  (interactive)
  (+workspace/close-window-or-workspace)
  (org-agenda-redo))

;; (add-hook 'org-journal-mode-hook
;;           (lambda ()
;;             (define-key org-journal-mode-map
;;               (kbd "C-x C-s") 'org-journal-save-entry-and-exit)))

(defun org-journal-file-header-func (time)
  "Custom function to create journal header."
  (concat
   (pcase org-journal-file-type
     (`daily (format-time-string "#+TITLE: %Y-%m-%d (%A)\n#+SETUPFILE: .local/journal_include.org\n#+FILETAGS: journal_daily_%Y%m%d_file\n#+STARTUP: folded\n\n"))
     (`weekly (format-time-string "#+TITLE: %Y_%W\n#+SETUPFILE: .local/journal_include.org\n#+FILETAGS: journal_weekly_%W_file\n#+STARTUP: content\n\n\n"))
     (`monthly (format-time-string "#+TITLE: %Y-%m (%B)\n#+SETUPFILE: .local/journal_include.org\n#+FILETAGS: journal_monthly_%Y%m_file\n#+STARTUP: content\n\nd"))
     (`yearly (format-time-string "#+TITLE: %Y\n#+SETUPFILE: .local/journal_include.org\n#+FILETAGS: journal_yearly_%Y_file\n#+STARTUP: content\n\n")))))

(setq org-journal-file-header 'org-journal-file-header-func)

(use-package! org-pandoc-import :after org)

(use-package! org-mind-map
  :init
  (require 'ox-org)
  :config
  (setq org-mind-map-engine "dot"))

(use-package! org-media-note
  :hook (org-mode .  org-media-note-setup-org-ref)
  :bind (
         ("C-c u m" . org-media-note-hydra/body))  ;; Main entrance
  :config
  (setq org-media-note-screenshot-image-dir ub/org-media-tmp-images)  ;; Folder to save screenshot
  (setq org-media-note-use-refcite-first t)  ;; use videocite link instead of video link if possible
  )

(setq org-modules '(
                    ;;org-attach
                    org-habit
                    org-mouse
                    org-protocol
                    org-annotate-file
                    org-eval
                    org-crypt
                    org-toc))

;; org-protocol
(require 'server)
(or (server-running-p)
    (server-start))
(require 'org-protocol)
(setq org-protocol-default-template-key "l")

;; two methods for attaching materials: org-download and org-attach

;; 1. org-download works as drag and drop, will create folder based on header
;; name. It is easy to use for quick attachments, but it might hard to keep the
;; folder structure same if you keep changing the names of header. On the other,
;; it is easier to find the file in folder since folder names are meaningful. It
;; might be useful quick drag and drop to files in download folder.
(use-package! org-download
  :config
  ;; take an image that is already on the clipboard
                                        ;(customize-set-variable 'org-download-screenshot-method "flameshot gui --raw > %s")
  (setq org-download-screenshot-method "export filename=\"%s\"; import png:\"$filename\" ;xclip -selection clipboard -target image/png -filter < \"$filename\" &>/dev/null")
  :config
  (setq org-download-timestamp "%Y%m%d%H%M%S_")
  ;; org-download-method code snippet taken from
  ;; https://github.com/jethrokuan/dots/blob/master/.doom.d/config.el
  (defun +org/org-download-method (link)
    (let* ((filename
            (file-name-nondirectory
             (car (url-path-and-query
                   (url-generic-parse-url link)))))
           ;; Create folder name with current buffer name, and place in root dir
           (dirname (concat
                     (replace-regexp-in-string " " "_" (downcase (file-name-base buffer-file-name)))
                     ".assets/"))
           (filename-with-timestamp (format "%s%s.%s"
                                            (format-time-string org-download-timestamp)
                                            (file-name-sans-extension filename)
                                            (file-name-extension filename))))
      (make-directory dirname t)
      (expand-file-name filename-with-timestamp dirname)))
  :config
  (setq org-download-method '+org/org-download-method)
  (setq org-image-actual-width nil) ;; think necessary for 500 to take effect
  (setq org-download-image-org-width 600)
  (setq org-download-annotate-function 'ignore)
  (setq org-download-annotate-function (lambda (_link) ""))
  ;;(setq org-download-heading-lvl 2)
  )

;; (defun org-download--dir-1 ()
;;  (or org-download-image-dir (concat (file-name-sans-extension (buffer-file-name)) ".assets")))
;; (setq org-download-method 'drestivo/org-download-method)
                                        ;(setq org-download-method +org/org-download-method)

;;(setq org-log-done 'time)
(setq org-log-into-drawer t)
;;(setq org-log-reschedule t)
(setq org-log-redeadline t)

(setq org-tags-exclude-from-inheritance
      (quote
       ("project_title" "horizon_title"
        "goal" "habit" "recurring" "focus_task" "reminder" "pinned"
        "important" "urgent"
        "r@batch" "r@daily" "r@weekly" "r@biweekly" "r@monthly" "r@quarterly" "r@emesterly" "r@yearly" "r@custom"
        "h@batch" "h@daily" "h@weekly" "h@biweekly" "h@monthly" "h@quarterly" "h@semesterly" "h@yearly" "h@custom"
        )))
;;(setq ub/tags-main-hide ".*_file\\|.*_agenda\\|project_title\\|project\\|r@.*\\|h@.*\\|j@schedule\\|j@morning\\|j@metric")
(setq ub/tags-main-hide ".*_file\\|.*_agenda\\|project_title\\|project\\|recurring\\|habit\\|gtd")
(setq org-agenda-hide-tags-regexp ub/tags-main-hide)
(setq org-archive-subtree-add-inherited-tags t)

;; agenda refile targets
(setq org-reverse-note-order t)
;; TODO define maxlevels for each refile file instead of all
(setq org-refile-targets `(
                           (,ub/refile-target-all-files-list :maxlevel . 2)
                           ))
;; useful for refile by structure of the files
(setq org-refile-use-outline-path t)
(setq org-refile-allow-creating-parent-nodes 'confirm) ; allow to create new headline

;; source https://github.com/alphapapa/unpackaged.el#refile-to-datetree-file-using-earliestlatest-timestamp-in-entry
;;;###autoload
(defun unpackaged/org-refile-to-datetree-using-ts-in-entry (which-ts file &optional subtree-p)
  "Refile current entry to datetree in FILE using timestamp found in entry.
WHICH should be `earliest' or `latest'. If SUBTREE-P is non-nil,
search whole subtree."
  (interactive (list (intern (completing-read "Which timestamp? " '(earliest latest)))
                     (read-file-name "File: " (concat org-directory "/") nil 'mustmatch nil
                                     (lambda (filename)
                                       (string-suffix-p ".org" filename)))
                     current-prefix-arg))
  (require 'ts)
  (let* ((sorter (pcase which-ts
                   ('earliest #'ts<)
                   ('latest #'ts>)))
         (tss (unpackaged/org-timestamps-in-entry subtree-p))
         (ts (car (sort tss sorter)))
         (date (list (ts-month ts) (ts-day ts) (ts-year ts))))
    (unpackaged/org-refile-to-datetree file :date date)))

;;;###autoload
(defun unpackaged/org-timestamps-in-entry (&optional subtree-p)
  "Return timestamp objects for all Org timestamps in entry.
 If SUBTREE-P is non-nil (interactively, with prefix), search
 whole subtree."
  (interactive (list current-prefix-arg))
  (save-excursion
    (let* ((beg (org-entry-beginning-position))
           (end (if subtree-p
                    (org-end-of-subtree)
                  (org-entry-end-position))))
      (goto-char beg)
      (cl-loop while (re-search-forward org-tsr-regexp-both end t)
               collect (ts-parse-org (match-string 0))))))

;;;###autoload
(cl-defun unpackaged/org-refile-to-datetree (file &key (date (calendar-current-date)) entry)
  "Refile ENTRY or current node to entry for DATE in datetree in FILE."
  (interactive (list (read-file-name "File: " "~/tmp" nil 'mustmatch nil
                                     (lambda (filename)
                                       (string-suffix-p ".org" filename)))))
  ;; If org-datetree isn't loaded, it will cut the tree but not file
  ;; it anywhere, losing data. I don't know why
  ;; org-datetree-file-entry-under is in a separate package, not
  ;; loaded with the rest of org-mode.
  (require 'org-datetree)
  (unless entry
    (org-cut-subtree))
  ;; Using a condition-case to be extra careful. In case the refile
  ;; fails in any way, put cut subtree back.
  (condition-case err
      (with-current-buffer (or (org-find-base-buffer-visiting file)
                               (find-file-noselect file))
        (org-datetree-file-entry-under (or entry (car kill-ring)) date)
        (save-buffer))
    (error (unless entry
             (org-paste-subtree))
           (message "Unable to refile! %s" err))))

;; (require 'epa-file)
;; (epa-file-enable)

(use-package! org-crypt
  :after org
  :config
  (org-crypt-use-before-save-magic)
  ;; (setq org-tags-exclude-from-inheritance (quote ("crypt")))  ;; BUG overwrites above tag-exclude list, use add-to-list
  :custom
  (org-crypt-key ub/org-crypt-key))

;; capture templates with doct
(use-package! doct
  ;;recommended: defer until calling doct
  :commands (doct))

;; org-gcal-sync doesn't work with capture
;; alternative method is to go to the captured location in the file and post-at-point
(defun my/gcal-capture-hook-post-at-point  ()
  (org-capture-goto-last-stored)
  (org-gcal-post-at-point))
(defun ub/gcal-capture-hook-sync  ()
  (org-gcal-sync)
  )

(defun ub/create-new-file ()
  "Create an org file in ~/tmp/."
  (interactive)
  (let ((name (read-string "Filename: ")))
    (expand-file-name (format "%s.org"
                              name) "~/tmp/")))


(defun ub/org-capture-auto-tag ()
  (interactive)
  (let ((alltags (append org-tag-persistent-alist (org-get-buffer-tags)))
        (headline-words (split-string (downcase (org-get-heading t t)))))
    (mapcar (lambda (word) (if (assoc (replace-regexp-in-string "[^[:alnum:]_-]"  "" word) alltags)
                               (org-toggle-tag (replace-regexp-in-string "[^[:alnum:]_-]"  "" word) 'on)))
            headline-words))
  (counsel-org-tag)
  )

;;(setq org-journal-todays-heading (format-time-string "%Y-%m-%d %A"))

(defun org-journal-goto-end ()
  (interactive)
  (save-excursion)
  ;; BUG throws an extra window which I cannot how to close it atm, this happens after mid night!
  ;; TODO create a condition use below function where journal file doesn't exist to create
  ;; TODO try to come up with a way to ub/journal-today file to be kept as yesterday after midnight up to 3-4 am
  (org-journal-new-entry t))


;; https://mollermara.com/blog/Fast-refiling-in-org-mode-with-hydras/
(defun ub/capture-project-refile (&optional arg)
  (setq org-refile-targets `(
                             (,ub/phd-projects-main-files-list :maxlevel . 1)
                             (,ub/personal-projects-main-files-list :maxlevel . 1)
                             (,ub/proaut-projects-main-files-list :maxlevel . 1)
                             ))
  (save-excursion)
  (org-id-get-create)
  ;;(org-id-update-id-locations)
  (org-refile)
  (switch-to-buffer (current-buffer)))


(defun ub/capture-area-refile (&optional arg)
  (setq org-refile-targets `(
                             (,ub/phd-areas-files-list :maxlevel . 1)
                             (,ub/personal-areas-files-list :maxlevel . 1)
                             (,ub/proaut-areas-files-list :maxlevel . 1)
                             ))
  (save-excursion)
  (org-id-get-create)
  ;;(org-id-update-id-locations)
  (org-refile)
  (switch-to-buffer (current-buffer)))



(defun ub/capture-id-create (&optional arg)
  (save-excursion)
  (org-id-get-create))

(defun ub/capture-id-create-and-demote (&optional arg)
  (save-excursion)
  (org-id-get-create)
  (org-do-demote))

(defun ub/capture-demote (&optional arg)
  (save-excursion)
  (org-do-demote))

(defun ub/capture-id-create-and-demote-and-pomodoro (&optional arg)
  (save-excursion)
  (org-id-get-create)
  (org-do-demote)
  (org-pomodoro))

;; http://doc.norang.ca/org-mode.html
;; (defun bh/remove-empty-drawer-on-clock-out ()
;;   (interactive)
;;   (save-excursion
;;     (beginning-of-line 0)
;;     (org-remove-empty-drawer-at "LOGBOOK" (point))))

;; (add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

(setq org-capture-templates
      (doct `(
              ("Task" :keys "t" :type entry :prepend nil
               :file ,ub/gtd-refile-file
               :before-finalize ub/capture-id-create
               :template ("* TODO %i%?  "
                          ":PROPERTIES:"
                          ":CREATED: %U"
                          ":REF-LINK: %a"
                          ":END:"))
              ("Task - More" :keys "T" :file "" :type entry :prepend nil
               :children (("Important and Urgent"  :keys "1"
                           :file ,ub/gtd-refile-file
                           :before-finalize ub/capture-id-create
                           :template ("* TODO [#A] %i%?    :urgent:important:"
                                      "SCHEDULED: %t DEADLINE: %^{Deadline Date}t"
                                      ":PROPERTIES:"
                                      ":CREATED: %U"
                                      ":REF-LINK: %a"
                                      ":END:"))
                          ("Important"  :keys "2"
                           :file ,ub/gtd-refile-file
                           :before-finalize ub/capture-id-create
                           :template ("* TODO [#A] %i%?   :important:"
                                      "SCHEDULED: %^{Schedule Date}t"
                                      ":PROPERTIES:"
                                      ":CREATED: %U"
                                      ":REF-LINK: %a"
                                      ":END:"))
                          ("Urgent"  :keys "3"
                           :file ,ub/gtd-refile-file
                           :before-finalize ub/capture-id-create
                           :template ("* TODO %i%?   :urgent:"
                                      "DEADLINE: %^{Deadline Date}t"
                                      ":PROPERTIES:"
                                      ":CREATED: %U"
                                      ":REF-LINK: %a"
                                      ":END:"))
                          ("Don't Do"  :keys "4"
                           :file ,ub/gtd-refile-file
                           :before-finalize ub/capture-id-create
                           :template ("* DONT %i%?   "
                                      ":PROPERTIES:"
                                      ":CREATED: %U"
                                      ":REF-LINK: %a"
                                      ":END:"))
                          ("Now Pomodoro" :keys "5" :type entry
                           ;;:clock-in t :clock-keep t
                           :file ,ub/journal-current-file
                           :function org-journal-goto-end
                           :jump-to-captured t
                           :before-finalize ub/capture-id-create-and-demote-and-pomodoro
                           :template ("* TODO %i%?  "
                                      ":PROPERTIES:"
                                      ":CREATED: %U"
                                      ":END:"))
                          ("Today" :keys "6" :type entry
                           :file ,ub/gtd-refile-file
                           :before-finalize ub/capture-id-create
                           :template ("* TODO %i%?  "
                                      "SCHEDULED: %t"
                                      ":PROPERTIES:"
                                      ":CREATED: %U"
                                      ":END:"))
                          ("Bug" :keys "7" :type entry
                           :file ,ub/gtd-refile-file
                           :before-finalize ub/capture-id-create
                           :template ("* BUG %i%?   "
                                      ":PROPERTIES:"
                                      ":CREATED: %U"
                                      ":REF-LINK: %a"
                                      ":END:"))
                           ("Review " :keys "8" :type entry
                           :clock-in t :clock-keep t
                           :file ,ub/journal-current-file
                           :before-finalize ub/capture-id-create-and-demote
                           :function org-journal-goto-end
                           :children (
                                      ("Weekly Review - Personal"  :keys "1"
                                       :template-file ,ub/me-weekly-review-template-file)
                                      ("Weekly Review - PhD"  :keys "2"
                                       :template-file ,ub/work-weekly-review-template-file)))
                          ("Plan" :keys "9" :type entry
                           :clock-in t :clock-keep t
                           :file ,ub/journal-current-file
                           :before-finalize ub/capture-id-create
                           :function org-journal-goto-end
                           :children (
                                      ("Weekly Meal"  :keys "1"
                                       :template-file ,ub/me-weekly-meal-template-file)
                                      ("Weekly Goal"  :keys "2"
                                       :template-file ,ub/me-weekly-goal-template-file)
                                      ))
                          ("Morning Journal"  :keys "0" :type plain
                           :file ,ub/journal-current-file
                           :function org-journal-goto-end
                           :template-file ,ub/me-morning-journal-template-file)
                          ))
              ("Note" :keys "n" :type entry  :prepend nil
               ;; :clock-in t :clock-resume t
               :file ,ub/gtd-refile-file
               :before-finalize ub/capture-id-create
               :template ("* NOTE %i%? "
                          ":PROPERTIES:"
                          ":CREATED: %U"
                          ":REF-LINK: %a"
                          ":END:"
                          ))
              ("Note - More" :keys "N" :type entry :prepend nil
               ;; :clock-in t :clock-resume t
               :file ,ub/gtd-refile-file
               :before-finalize ub/capture-id-create
               :children (
                          ("Idea" :keys "1"
                           :template ("* NOTE Idea: %i%?  💡"
                                      ":PROPERTIES:"
                                      ":CREATED: %U"
                                      ":REF-LINK: %a"
                                      ":END:"))
                          ("Question" :keys "2"
                           :template ("* NOTE Question: %i%?  :question:"
                                      ":PROPERTIES:"
                                      ":CREATED: %U"
                                      ":REF-LINK: %a"
                                      ":END:"
                                      "%i"))
                          ("Decision" :keys "3"
                           :template ("* NOTE Decision: %i%?  :decison:"
                                      ":PROPERTIES:"
                                      ":CREATED: %U"
                                      ":REF-LINK: %a"
                                      ":END:"))
                          ("Lesson Learned" :keys "4"
                           :template ("* NOTE Lesson learned: %i%?  :lesson_learned:"
                                      ":PROPERTIES:"
                                      ":CREATED: %U"
                                      ":REF-LINK: %a"
                                      ":END:"))
                          ("Quote" :keys "5"
                           :template ("* NOTE Quote: %i%?  :quote:"
                                      ":PROPERTIES:"
                                      ":CREATED: %U"
                                      ":REF-LINK: %a"
                                      ":END:"))
                          ("Whiteboard" :keys "6"
                           :template ("* NOTE Whiteboard %i%?  :whiteboard:"
                                      ":PROPERTIES:"
                                      ":CREATED: %U"
                                      ":REF-LINK: %a"
                                      ":END:"))
                          ("WTF" :keys "7"
                           :template ("* NOTE WTF %i%?   :wtf:"
                                      ":PROPERTIES:"
                                      ":CREATED: %U"
                                      ":REF-LINK: %a"
                                      ":END:"))
                          ("Meditation" :keys "8"
                           :template ("* NOTE Meditation %i%?  :j@meditation:"
                                      ":PROPERTIES:"
                                      ":CREATED: %U"
                                      ":END:"))
                          ("Dream" :keys "9"
                           :template ("* NOTE Dream %i%?     :j@dream:"
                                      ":PROPERTIES:"
                                      ":CREATED: %U"
                                      ":REF-LINK: %a"
                                      ":END:"))
                          ))
              ("Journal/Interruption" :keys "j" :type entry :prepend nil
               :clock-in t :clock-resume t
               :file ,ub/journal-current-file
               :before-finalize ub/capture-demote
               :function org-journal-goto-end
               :template ("* %i%?"
                          ":PROPERTIES:"
                          ":CREATED: %U"
                          ":REF-LINK: %a"
                          ":END:"))
              ("Log/Metric" :keys "l" :type entry :prepend nil
               ;;:clock-in t :clock-resume t
               :file ,ub/journal-current-file
               :before-finalize ub/capture-demote
               :function org-journal-goto-end
               :children (
                          ("Morning Mood"  :keys "1"
                           :template-file ,ub/me-morning-mood-log-template-file)
                          ("Sleep"  :keys "2"
                           :template-file ,ub/me-sleep-log-template-file)
                          ("Reflection Mood"  :keys "3"
                           :template-file ,ub/me-reflection-log-template-file)
                          ("Current Mood" :keys "4"
                           :template-file ,ub/me-current-mood-log-template-file)
                          ("Exercise" :keys "5"
                           :template-file ,ub/me-exercise-log-template-file)
                          ))
              ("Schedule" :keys "s" :file ,ub/journal-current-file :type plain :prepend nil
               :function org-journal-goto-end
               :after-finalize (lambda () (org-agenda-redo))
               :warn nil ;; :start-time used in template-file so disable warning
               :children (
                          ("Custom"  :keys "0"
                           :custom-block (lambda () (setq ub/custom-description (read-string "Description: ")))
                           :custom-block-var  (lambda () ub/custom-description)
                           :start-time (lambda () (let* ((ts (ts-parse (setq ub/start-time-entered (org-time-stamp nil))))) (ts-format "%H:%M" ts)))
                           :end-time (lambda () (let* ((ts (ts-parse ub/start-time-entered))) (ts-format "%H:%M" (ts-adjust 'hour (read-number "Duration(h): " 1) ts))))
                           :template-file ,ub/me-custom-block-schedule-template-file)
                          ("Morning"  :keys "1"
                           ;;:hook (lambda () (setq ub/schedule-template-start-time (read-string "start time:")) )
                           :start-time (lambda () (let* ((ts (ts-parse (setq ub/start-time-entered (org-time-stamp nil))))) (ts-format "%H:%M" ts)))
                           :start-time-30 (lambda () (let* ((ts (ts-parse ub/start-time-entered))) (ts-format "%H:%M" (ts-adjust 'minute 30 ts))))
                           :start-time-3-30 (lambda () (let* ((ts (ts-parse ub/start-time-entered))) (ts-format "%H:%M" (ts-adjust 'hour 3 'minute 30 ts))))
                           :start-time-5-30 (lambda () (let* ((ts (ts-parse ub/start-time-entered))) (ts-format "%H:%M" (ts-adjust 'hour 5 'minute 30 ts))))
                           :start-time-5-45 (lambda () (let* ((ts (ts-parse ub/start-time-entered))) (ts-format "%H:%M" (ts-adjust 'hour 5 'minute 45 ts))))
                           :template-file ,ub/me-morning-schedule-template-file)
                          ("Afternoon"  :keys "2"
                           :start-time (lambda () (let* ((ts (ts-parse (setq ub/start-time-entered (org-time-stamp nil))))) (ts-format "%H:%M" ts)))
                           :start-time-3 (lambda () (let* ((ts (ts-parse ub/start-time-entered))) (ts-format "%H:%M" (ts-adjust 'hour 3 ts))))
                           :start-time-4 (lambda () (let* ((ts (ts-parse ub/start-time-entered))) (ts-format "%H:%M" (ts-adjust 'hour 4 ts))))
                           :start-time-6 (lambda () (let* ((ts (ts-parse ub/start-time-entered))) (ts-format "%H:%M" (ts-adjust 'hour 6 ts))))
                           :start-time-8 (lambda () (let* ((ts (ts-parse ub/start-time-entered))) (ts-format "%H:%M" (ts-adjust 'hour 8 ts))))
                           :template-file ,ub/me-afternoon-schedule-template-file)
                          ("Deep Block"  :keys "3"
                           :start-time (lambda () (let* ((ts (ts-parse (setq ub/start-time-entered (org-time-stamp nil))))) (ts-format "%H:%M" ts)))
                           :end-time (lambda () (let* ((ts (ts-parse ub/start-time-entered))) (ts-format "%H:%M" (ts-adjust 'hour (read-number "Duration(h): " 5) ts))))
                           :template-file ,ub/me-deep-block-schedule-template-file)
                          ("Shallow Block"  :keys "4"
                           :start-time (lambda () (let* ((ts (ts-parse (setq ub/start-time-entered (org-time-stamp nil))))) (ts-format "%H:%M" ts)))
                           :end-time (lambda () (let* ((ts (ts-parse ub/start-time-entered))) (ts-format "%H:%M" (ts-adjust 'hour (read-number "Duration(h): " 3) ts))))
                           :template-file ,ub/me-shallow-block-schedule-template-file)
                          ("Reflection Block"  :keys "5"
                           :start-time (lambda () (let* ((ts (ts-parse (setq ub/start-time-entered (org-time-stamp nil))))) (ts-format "%H:%M" ts)))
                           :end-time (lambda () (let* ((ts (ts-parse ub/start-time-entered))) (ts-format "%H:%M" (ts-adjust 'hour (read-number "Duration(h): " 2) ts))))
                           :template-file ,ub/me-reflection-block-schedule-template-file)
                          ("Exercise Block"  :keys "6"
                           :start-time (lambda () (let* ((ts (ts-parse (setq ub/start-time-entered (org-time-stamp nil))))) (ts-format "%H:%M" ts)))
                           :end-time (lambda () (let* ((ts (ts-parse ub/start-time-entered))) (ts-format "%H:%M" (ts-adjust 'hour (read-number "Duration(h): " 1) ts))))
                           :template-file ,ub/me-exercise-block-schedule-template-file)
                          ("Lunch Block"  :keys "7"
                           :start-time (lambda () (let* ((ts (ts-parse (setq ub/start-time-entered (org-time-stamp nil))))) (ts-format "%H:%M" ts)))
                           :end-time (lambda () (let* ((ts (ts-parse ub/start-time-entered))) (ts-format "%H:%M" (ts-adjust 'hour (read-number "Duration(h): " 1) ts))))
                           :template-file ,ub/me-lunch-block-schedule-template-file)
                          ("Dinner Block"  :keys "8"
                           :start-time (lambda () (let* ((ts (ts-parse (setq ub/start-time-entered (org-time-stamp nil))))) (ts-format "%H:%M" ts)))
                           :end-time (lambda () (let* ((ts (ts-parse ub/start-time-entered))) (ts-format "%H:%M" (ts-adjust 'hour (read-number "Duration(h): " 2) ts))))
                           :template-file ,ub/me-dinner-block-schedule-template-file)
                          ("Meditation Block"  :keys "9"
                           :start-time (lambda () (let* ((ts (ts-parse (setq ub/start-time-entered (org-time-stamp nil))))) (ts-format "%H:%M" ts)))
                           :end-time (lambda () (let* ((ts (ts-parse ub/start-time-entered))) (ts-format "%H:%M" (ts-adjust 'minute (read-number "Duration(min): " 20) ts))))
                           :template-file ,ub/me-meditation-block-schedule-template-file)
                          ("Daily Schedule - Basic"  :keys "a"
                           :template-file ,ub/me-daily-schedule-basic-template-file)
                          ("Daily Schedule - Focus"  :keys "b"
                           :template-file ,ub/me-daily-schedule-focus-template-file)
                          ("Daily Schedule - Reasonable for Depression"  :keys "c"
                           :template-file ,ub/me-daily-schedule-reasonable-for-depression-template-file)
                          ))
              ("Appointment/Meeting" :keys "a" :prepend nil
               :after-finalize ub/gcal-capture-hook-sync
               :file ,ub/gcal-appointment-file
               :template (,ub/capture-appointment-template))
              ("Web" :keys "w" :file ""
               :children (
                          ("Paper" :keys "p" :type entry :prepend t :olp ("Papers" "Inbox")
                           :file ,ub/gtd-work-file
                           :hook ub/org-capture-auto-tag
                           :template ("* %a %? "
                                      ":PROPERTIES:"
                                      ":CREATED: %U"
                                      ":REF-LINK: %a"
                                      ":END:"
                                      "%i"))
                          ("Leisure" :keys "e" :type entry :prepend t
                           :file ,ub/gtd-leisure-file :immediate-finish t
                           :children (
                                      ("Movie" :keys "1" :olp ("Movies" "Inbox")
                                       :template ("* %a %?"
                                                  ":PROPERTIES:"
                                                  ":CREATED: %U"
                                                  ":REF-LINK: %a"
                                                  ":END:"
                                                  "%i"))
                                      ("Series" :keys "2" :olp ("Series" "Inbox")
                                       :template ("* %a %?"
                                                  ":PROPERTIES:"
                                                  ":CREATED: %U"
                                                  ":REF-LINK: %a"
                                                  ":END:"
                                                  "%i"))
                                      ("Podcast/Talk" :keys "3" :olp ("Podcasts/Talks" "Inbox")
                                       :template ("* %a %?"
                                                  ":PROPERTIES:"
                                                  ":CREATED: %U"
                                                  ":REF-LINK: %a"
                                                  ":END:"
                                                  "%i"))
                                      ("Book" :keys "4" :olp ("Books" "Inbox")
                                       :template ("* %a %?"
                                                  ":PROPERTIES:"
                                                  ":CREATED: %U"
                                                  ":REF-LINK: %a"
                                                  ":END:"
                                                  "%i"))
                                      ("CS & Programming" :keys "5" :olp ("CS & Programming" "Inbox")
                                       :template ("* %a %?"
                                                  ":PROPERTIES:"
                                                  ":CREATED: %U"
                                                  ":REF-LINK: %a"
                                                  ":END:"
                                                  "%i"))
                                      ))
                          ("Music" :keys "M" :type entry :prepend t :immediate-finish t
                           :file ub/gtd-leisure-file
                           ;;:hook ub/org-capture-auto-tag
                           :children (
                                      ("Track" :keys "t" :olp ("Music" "Tracks" "Inbox")
                                       :template ("* %a %?"
                                                  ":PROPERTIES:"
                                                  ":CREATED: %U"
                                                  ":REF-LINK: %a"
                                                  ":END:"
                                                  "%i"))
                                      ("Set" :keys "s" :olp ("Music" "Sets" "Inbox")
                                       :template ("* %a %?"
                                                  ":PROPERTIES:"
                                                  ":CREATED: %U"
                                                  ":REF-LINK: %a"
                                                  ":END:"
                                                  "%i"))
                                      ("Label" :keys "l" :olp ("Music" "Label" "Inbox")
                                       :template ("* %a %?"
                                                  ":PROPERTIES:"
                                                  ":CREATED: %U"
                                                  ":REF-LINK: %a"
                                                  ":END:"
                                                  "%i"))
                                      ))
                          ("Content" :keys "c" :clock t :type plain :immediate-finish t
                           :template ("%i"
                                      "%a"))
                          ("Interesting Stuff" :keys "P"
                           :olp ("Interesting Stuff" "Inbox")
                           :file ,ub/gtd-leisure-file :type entry :immediate-finish t
                           :template ("* %a"
                                      ":PROPERTIES:"
                                      ":CREATED: %U"
                                      ":REF-LINK: %a"
                                      ":END:"
                                      "%i"))))
              ))
      )

(require 'org-id)
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
(use-package! org-super-links
  :bind* (("C-c s s" . org-super-links-link)
          ("C-c s l" . org-super-links-store-link)
          ("C-c s C-l" . org-super-links-insert-link)))

(use-package! org-mru-clock
  :after ivy
  :bind* (("C-c C-x i" . org-mru-clock-in)
          ("C-c C-x C-j" . org-mru-clock-select-recent-task))
  :config
  (setq org-mru-clock-how-many 100
        org-mru-clock-completing-read #'ivy-completing-read)
  (add-hook 'minibuffer-setup-hook #'org-mru-clock-embark-minibuffer-hook))
(setq org-clock-mode-line-total 'today)
(setq org-duration-format (quote h:mm))

(use-package! org-toggl
  :after org-clock
  ;;:straight (org-toggl :repo "git@github.com:Fuco1/org-toggl.git")
  :custom
  (org-toggl-inherit-toggl-properties t)
  :config
  (setq toggl-auth-token ub/toggl-auth-token)
  (toggl-get-projects)
  (org-toggl-integration-mode))

(require 'org-archive)

; Set the function to use for org-archive-default  (C-c C-x C-a)
; (setq org-archive-location (concat org-directory "/archive/%s_archive::"))
; (setq org-archive-location "archive/archived_%s::")
; (setq org-archive-location "::* ARCHIVED")

; unmap org-archive-subtree
(define-key org-mode-map (kbd "C-c C-x C-s") nil)

; select command to execute via org-archive-subtree-default (C-c C-x C-a)
(setq org-archive-default-command 'org-archive-subtree-hierarchical)

(defun line-content-as-string ()
  "Returns the content of the current line as a string"
  (save-excursion
    (beginning-of-line)
    (buffer-substring-no-properties
     (line-beginning-position) (line-end-position))))

(defun org-child-list (&optional top-level)
  "This function returns all children of a heading as a list. "
  (interactive)
  (save-excursion
    ;; this only works with org-version > 8.0, since in previous
    ;; org-mode versions the function (org-outline-level) returns
    ;; gargabe when the point is not on a heading.
    (unless top-level
        (if (= (org-outline-level) 0)
            (outline-next-visible-heading 1)
        (org-goto-first-child)))
    (let ((child-list (list (line-content-as-string))))
      (while (org-goto-sibling)
        (setq child-list (cons (line-content-as-string) child-list)))
      child-list)))

(defun fa/org-struct-subtree ()
  "This function returns the tree structure in which a subtree belongs as a list."
  (interactive)
  (let ((archive-tree nil))
    (save-excursion
      (while (org-up-heading-safe)
        (let ((heading
               (buffer-substring-no-properties
                (line-beginning-position) (line-end-position))))
          (if (eq archive-tree nil)
              (setq archive-tree (list heading))
            (setq archive-tree (cons heading archive-tree))))))
    archive-tree))

(defun org-archive-subtree-hierarchical ()
  "This function archives a subtree hierarchical"
  (interactive)
  (let ((org-tree (fa/org-struct-subtree))
        (source-buffer (current-buffer))
        (file (abbreviate-file-name
                   (or (buffer-file-name (buffer-base-buffer))
                       (error "No file associated to buffer")))))
    (save-excursion
      (setq location (org-archive--compute-location
                (or (org-entry-get nil "ARCHIVE" 'inherit)
                    org-archive-location))
            afile (car location)
            heading (cdr location)
            infile-p (equal file (abbreviate-file-name (or afile ""))))
      (unless afile
        (error "Invalid `org-archive-location'"))
      (if (not (equal heading ""))
          (progn
            (setq org-tree (cons heading
                               (mapcar (lambda (s) (concat "*" s)) org-tree)))
            (org-demote-subtree)))
      (if (> (length afile) 0)
        (progn
          (setq newfile-p (not (file-exists-p afile))
                visiting (find-buffer-visiting afile)
                target-buffer (or visiting (find-file-noselect afile))))
        (progn
          (setq target-buffer (current-buffer))))
      (unless target-buffer
        (error "Cannot access file \"%s\"" afile))
      (org-cut-subtree)
      (set-buffer target-buffer)
      (setq ind-target-buffer (clone-indirect-buffer nil nil))
      (set-buffer ind-target-buffer)
      (org-mode)
      (goto-char (point-min))

      ; simplified version of org-complex-heading-regexp-format
	  (setq my-org-complex-heading-regexp-format
	      (concat "^"
		      "\\(%s\\)"
		      "\\(?: *\\[[0-9%%/]+\\]\\)*"
		      "\\(?:[ \t]+\\(:[[:alnum:]_@#%%:]+:\\)\\)?"
		      "[ \t]*$"))
      (setq top-level-p t)
      (while (not (equal org-tree nil))
        (let ((child-list (org-child-list top-level-p))
              (re (format my-org-complex-heading-regexp-format (regexp-quote (car org-tree))))
             )
          (if (member "______FOUND_MATCH" (mapcar (lambda (s) (replace-regexp-in-string re "______FOUND_MATCH" s)) child-list))
              (progn
                (re-search-forward re nil t)
                (setq org-tree (cdr org-tree)))
            (progn
              (if (not top-level-p) (newline))
              (org-insert-struct org-tree)
              (setq org-tree nil))))
        (setq top-level-p nil))
      (newline)
      (org-yank)
      ;; Kill the indirect buffer, returning the current buffer to the direct target buffer
      ;;(kill-buffer ind-target-buffer)
      ;; Save and kill the target buffer, if it is not the source buffer.
      (when (not (eq source-buffer target-buffer))
            (save-buffer target-buffer)
            ;;(kill-buffer target-buffer)
            )
      ;; ensure font-lock and indentation are normal
      (set-buffer source-buffer)
      (org-restart-font-lock)
      (org-indent-mode t)
      (message "Subtree archived %s"
               (concat "in file: " (abbreviate-file-name afile))))))

(defun org-insert-struct (struct)
  "TODO"
  (interactive)
  (when struct
    (insert (car struct))
    (if  (not (equal (length struct) 1))
        (newline))
    (org-insert-struct (cdr struct))))

;; org-gcal
(use-package! org-gcal
  :config
  (setq org-gcal-recurring-events-mode 'nested
        org-gcal-auto-archive nil
        org-gcal-client-id ub/gcal-client-id-global
        org-gcal-token-file (expand-file-name ".org-gcal-token" ub/gcal-root-dir)
        org-gcal-client-secret ub/gcal-client-secret-global
        org-gcal-file-alist `(
                              (,ub/gcal-ugurbolatpersonal-link .  ,ub/gcal-ugurbolatpersonal-file)
                              (,ub/gcal-appointment-link .  ,ub/gcal-appointment-file)
                              ;; (,ub/gcal-personal-time-blocks-link .  ,ub/gcal-personal-time-blocks-file)
                              ;; (,ub/gcal-personal-default-daily-schedules-link .  ,ub/gcal-personal-default-daily-schedules-file)
                              (,ub/gcal-gtd-link . ,ub/gcal-gtd-file)
                              )))
;; NOTE breaks ub/org-agenda-dashboard
;;(add-hook 'org-agenda-mode-hook 'org-gcal-sync)

(setq org-pomodoro-length 30)
(setq org-pomodoro-short-break-length 5)
(setq org-pomodoro-long-break-frequency 4)
(setq org-pomodoro-start-sound-p t)
(setq org-pomodoro-finished-sound-p t)
(setq org-pomodoro-overtime-sound-p t)
(setq org-pomodoro-short-break-sound-p t)
(setq org-pomodoro-long-break-sound-p t)
(setq org-pomodoro-manual-break t)

;; org repeating task todo return state after done
(setq org-todo-repeat-to-state "T@NEXT")

;; TaskJuggler export customization
;; source: https://hugoideler.com/2018/09/org-mode-and-wide-taskjuggler-html-export/
(setq org-taskjuggler-default-reports
      '("textreport report \"Plan\" {
formats html
header '== %title =='
center -8<-
[#Plan Plan] | [#Resource_Allocation Resource Allocation]
----
=== Plan ===
<[report id=\"plan\"]>
----
=== Resource Allocation ===
<[report id=\"resourceGraph\"]>
->8-
}
# A traditional Gantt chart with a project overview.
taskreport plan \"\" {
headline \"Project Plan\"
columns bsi, name, start, end, effort, effortdone, effortleft, chart { width 1500 }
loadunit shortauto
hideresource 1
}
# A graph showing resource allocation. It identifies whether each
# resource is under- or over-allocated for.
resourcereport resourceGraph \"\" {
headline \"Resource Allocation Graph\"
columns no, name, effort, weekly { width 1000 }
loadunit shortauto
hidetask ~(isleaf() & isleaf_())
sorttasks plan.start.up
}")
      )

(setq org-taskjuggler-default-project-duration 999)
(setq org-taskjuggler-valid-task-attributes
      '(account start note duration endbuffer endcredit end
                flags journalentry length limits maxend maxstart minend
                minstart period reference responsible scheduling
                startbuffer startcredit statusnote chargeset charge booking))

;; org-super-agenda package
(use-package! org-super-agenda
  :after org
  :config
  (org-super-agenda-mode t)
  (require 'org-habit)
  )
(after! org-super-agenda
  ;; tag hierarchy and group don't work
  ;; a fix from https://github.com/alphapapa/org-super-agenda/issues/136
  ;; still doesn't work for tag grouping with regular expressions such as Project : {P@.+}
  (org-super-agenda--defgroup tag
    "Group items that match any of the given tags. Argument may be a string or list of strings."
    :let* ((target-tags (-flatten (cons args (mapcar (lambda (arg) (funcall #'org-tags-expand arg t)) args)))))
    :section-name (concat "Tags: " (s-join " OR " args))
    :test (seq-intersection (org-super-agenda--get-tags item) target-tags 'cl-equalp)))

(setq ub/org-agenda-files-all-except-journal-code-block
      `((org-agenda-files ',ub/gtd-all-files-list)))

(setq ub/org-agenda-files-all (append ub/gtd-all-files-list
                                      ub/journal-main-files-list
                                      ))
(setq ub/org-agenda-files-all-code-block
      `((org-agenda-files ',ub/org-agenda-files-all)))

(setq org-todo-keywords
      '(
        (sequence "TODO(T!)" "T@SOMEDAY(s!)" "T@NEXT(n@/!)" "T@HOLD(h@/!)" "T@IN-PROGRESS(i@/!)" "T@WAITING(w@/!)" "|" "T@DONE(d!)" "T@DELEGATE(g!)" "T@KILL(k!)")
        (sequence "IDEA(I!)" "I@SOMEDAY(s!)" "I@NEXT(n@/!)" "I@HOLD(h@/!)" "I@CONNECTED(i@/!)" "|" "I@DONE(d!)" "I@DELEGATE(g!)" "I@KILL(k!)")
        (sequence "DONT(D!)" "D@NEXT(n@/!)" "D@IN-PROGRESS(i@/!)" "|" "D@DONE(d!)" "D@KILL(k!)")
        (sequence "NOTE(N!)" "N@ORGZLY(z)" "N@NEXT(n!)" "N@WRITING(i!)" "|" "N@DONE(d!)" "N@KILL(k!)")
        ;;(sequence "QUESTION(Q!)" "|" "Q@DONE(E!)" "Q@KILL(X!)")
        ;;(sequence "READ(r!)" "R@SKIM(m!)" "R@ANALYTIC(a!)" "R@STUDY(s!)" "|" "R@DONE(1!)" "R@KILL(2!)")
        ;;(sequence "LEARN(l!)" "L@IN-PROGRESS(y@/!)" "|" "L@DONE(5!)" "L@KILL(6!)")
        (sequence "BUG(B!)" "B@SOMEDAY(s!)" "B@NEXT(n@/!)" "B@KNOWN-CAUSE(c@/!)" "B@REPORTED(r@/!)" "|" "B@FIXED(d!)" "B@WORKAROUND(o@)" "B@KILL(k!)" )
        (sequence "PROJECT(J!)" "J@SOMEDAY(s!)" "J@NEXT(n@/!)" "J@HOLD(h@/!)" "J@IN-PROGRESS(i@/!)" "|" "J@DONE(d!)" "J@KILL(k!)")
        ;;(sequence "POCKET(P)" "|" "P@DONE(d!)" "P@KILL(k!)")
        (sequence "|" "DONE(d!)" "KILL(k!)")
        ))

;; org-todo-keyword-faces
(custom-set-faces '(org-headline-done ((t (:inherit variable-pitch :strike-through t)))))

;; https://github.com/nmartin84/.doom.d
;; (custom-declare-face '+org-todo-next '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
;; (custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
;; (custom-declare-face '+org-todo-onhold  '((t (:inherit (bold warning org-todo)))) "")
;; (custom-declare-face '+org-todo-next '((t (:inherit (bold font-lock-keyword-face org-todo)))) "")
;; (custom-declare-face 'org-checkbox-statistics-todo '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
;; (custom-declare-face '+org-done '((t (:inherit (bold font-lock-constant-face org-done)))) "")

;;         (setq org-todo-keyword-faces
;;               '(
;;           ;;       ("WAIT" . +org-todo-onhold)
;;           ;; ("DOING" . +org-todo-active)
;;           ;; ("NEXT" . +org-todo-next)
;;           ;; ("REFILE" . +org-todo-onhold)
;;           ;; ("PROJ" . +org-todo-next)
;;           ("KILL" . +org-done)))

;; somehow KILL is same color with org-todo keyword
(setq org-todo-keyword-faces
      '(("KILL" . org-done)))

;;(defface org-todo-keyword-kill '((t ())) "org-kill" :group 'org)
;; (doom-themes-set-faces 'user
;;   ;; '(variable-pitch :font "New York")
;; ;; ** font-lock
;;   ;; '(font-lock-builtin-face              :foreground builtin :slant 'italic :weight 'light)
;;   ;; '(font-lock-variable-name-face        :foreground variables :weight 'semi-bold)
;;   ;; '(font-lock-function-name-face        :foreground functions :weight 'semi-bold)
;;   ;; '(font-lock-keyword-face              :foreground keywords :weight 'semi-bold)
;;   ;; '(font-lock-string-face               :foreground strings :weight 'semi-bold)
;;   ;; '(font-lock-type-face                 :foreground type :slant 'italic)
;;   '(org-done :foreground (doom-blend 'yellow 'bg 0.7)  :weight 'bold :height 1.0)
;;   )


;; NOTE disabled it because it only shows these tags. expected behaviour is to show them up in the tag list.
;; (setq org-tag-alist '(("important" . ?i)
;;                       ("urgent" . ?u)
;;                       ("shallow_task" . ?s)
;;                       ("deep_task" . ?d)
;;                       ))

;; prompts todo keywords as list
;; NOTE 'auto selection doesn't fit org-todo prompt window so set to 'expert
;; NOTE 2 shortened keywords list auto fits now
;; MAYBE https://emacs.stackexchange.com/questions/59424/org-set-effort-fast-effort-selection
(setq org-use-fast-todo-selection 'auto)

(setq org-agenda-files ub/org-agenda-files-all) ; default agenda file
(setq org-agenda-sorting-strategy '((agenda habit-down time-up priority-down category-keep)
                                    (todo todo-state-down priority-down timestamp-up)
                                    (tags todo-state-down priority-down timestamp-up)
                                    (search todo-state-down priority-down timestamp-up)))

(setq org-agenda-skip-deadline-prewarning-if-scheduled nil)
(setq org-agenda-start-on-weekday 1)
(setq calendar-week-start-day 1)
(setq org-agenda-start-day "+0d")
(setq org-agenda-sticky t)
(setq org-agenda-window-setup (quote current-window))
(setq org-deadline-warning-days 365)
(setq org-extend-today-until 4)
(setq org-use-effective-time t)
;;(setq org-habit-show-habits-only-for-today t)
;;(setq org-agenda-show-future-repeats 'next)

(setq org-agenda-prefix-format (quote ((agenda . " %i %-30:c%-12t% s")
                                       (todo . " %i %-30:c%-12t% s")
                                       (tags . "%i %-30:c%-12t% s")
                                       (search . " %i %-30:c%-12t% s"))))

(setq org-agenda-format-date "\n :calendar: %Y-%m-%d %A")
(setq org-agenda-archives-mode nil) ;; NOTE setting t break org-agenda :/
(setq org-super-agenda-date-format "%d %b %Y - %A")
(setq org-agenda-time-grid '((daily today require-timed remove-match) (1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200 2300) "......" "----------------"))
(setq org-agenda-current-time-string "now <- - - - - - - - - - - - - - -")
(setq org-agenda-show-current-time-in-grid t)
(setq org-agenda-log-mode-add-notes nil)
(setq org-agenda-clockreport-parameter-plist '(:stepskip0 t :link t :maxlevel 3 :fileskip0 t))

(setq ub/org-agenda-settings-report
      `(
        ;;(org-agenda-format-date "%Y-%m-%d %A")
        (org-agenda-show-all-dates t)
        (org-agenda-start-with-log-mode t)
        (org-agenda-show-log t)
        (org-agenda-time-grid nil)
        (org-agenda-log-mode-items '(clock))
        ;;(org-agenda-start-with-clockreport-mode t)
        (org-agenda-archives-mode nil)
        ;; I don't care if an entry was archived
        (org-agenda-hide-tags-regexp
         (concat org-agenda-hide-tags-regexp
                 "\\|ARCHIVE"))
        (org-super-agenda-groups
         '(
           (:discard (:file-path ".*_archive"))
           (:discard (:log nil))
           (:name "" :anything t)))

        ))

(setq ub/org-agenda-settings-habit-pinned
      `(
        (org-agenda-span 'day)
        ;;(org-agenda-block-separator nil)
        (org-agenda-format-date "")
        (org-agenda-time-grid nil)
        (org-habit-show-habits t)
        (org-habit-show-all-today t)
        ;; (org-habit-show-all-today t)
        ;;(org-deadline-warning-days 365)
        ;; (org-scheduled-past-days 0)
        (org-agenda-sorting-strategy '((agenda category-keep))) ; NOTE Doesn't work for sorting habits :/
        (org-agenda-hide-tags-regexp (concat ub/tags-main-hide "\\|pinned\\|h@.*\\|habit"))
        (org-agenda-overriding-header ":repeat: Pinned Habits")
        (org-super-agenda-groups
         '(
           (:name "" :order 2 :and (:todo "T@NEXT" :tag "habit" :tag "pinned"))
           ;;(:name "Daily" :order 4 :and (:todo "T@NEXT" :tag "h@daily"))
           ;;(:name "More" :order 3 :and (:todo "T@NEXT" :tag "habit" :scheduled today))
           (:discard (:anything t))))
        ))

(setq ub/org-agenda-settings-todays-schedule
      `(
        (org-agenda-overriding-header "🤙🏿 Today's Agenda")
        (org-agenda-format-date "")
        (org-agenda-span 1)
        (org-agenda-time-grid '((daily today) () "......" "----------------"))
        ;;(org-agenda-time-grid '((daily today remove-match) (1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200 2300) "......" "----------------"))
        (org-agenda-current-time-string "now <- - - - - - - - - - - - - - -")
        (org-agenda-show-current-time-in-grid t)
        (org-habit-show-habits t)
        (org-habit-show-habits-only-for-today t)
        ;;(org-habit-show-all-today t)
        (org-agenda-sorting-strategy '((agenda time-up habit-down priority-down category-keep)))
        (org-super-agenda-groups
         '(
           ;;(:discard (:tag "habit"))
           ;; NOTE pinned habits doesn't require scheduling since it has its own section
           (:discard (:and (:tag "habit" :tag "pinned")))
           (:discard (:tag "reminder"))
           (:discard (:tag "j@1todaygreat"))
           (:discard (:tag "j@tomorrowself"))
           (:discard (:todo "T@HOLD"))
           ;; NOTE filter all todos that doesn't have today's date - except non todo items which are fixed scheduled events like Lunch, Meditation etc. If you are going to do a todo item then schedule it today. Use reminder section to what can be scheduled today from todos
           (:discard (:and (:todo t :not (:date today))))
           ;; NOTE time-grid t doesn't work when filter with j@schedule so now -> isn't showed, only works with alone
           ;; (:discard (:and (:todo nil :not (:tag "fixed_schedule"))))
           (:name "" :order 1 :time-grid t)
           (:name "" :order 2 :date today)
           (:discard (:anything t))
           ))
        )
      )

(setq ub/org-agenda-settings-tasks-important-urgent-wo-date
      `(
        (org-agenda-overriding-header "")
        (org-deadline-warning-days 365)
        (org-agenda-sorting-strategy '((todo todo-state-down priority-down timestamp-up)))
        (org-agenda-hide-tags-regexp (concat ub/tags-main-hide "\\|important\\|urgent"))
        (org-super-agenda-groups
         '(
           ;; (:discard (:habit t))
           ;; (:discard (:tag "recurring"))
           ;; (:discard (:tag "project_title"))
           ;; (:discard (:date t))
           ;; (:discard (:scheduled t))
           ;; (:discard (:deadline t))
           (:name "Important & Urgent w/o Date"
            ;;:discard (:date t)
            :discard (:scheduled t)
            :discard (:deadline t)
            :and (:tag "important" :tag "urgent")
            :order 1)
           (:discard (:anything t))))
        ))

(setq ub/org-agenda-settings-tasks-important-wo-date
      `(
        (org-agenda-overriding-header "")
        (org-deadline-warning-days 365)
        (org-agenda-sorting-strategy '((todo todo-state-down priority-down timestamp-up)))
        (org-agenda-hide-tags-regexp (concat ub/tags-main-hide "\\|important\\|urgent"))
        (org-super-agenda-groups
         '(
           ;; (:discard (:habit t))
           ;; (:discard (:tag "recurring"))
           ;; (:discard (:tag "project_title"))
           ;; (:discard (:date t))
           ;; (:discard (:scheduled t))
           ;; (:discard (:deadline t))
           (:name "Important w/o Date"
            ;;:discard (:date t)
            :discard (:scheduled t)
            :discard (:deadline t)
            :discard (:tag "urgent")
            :and (:tag "important")
            :order 1)
           (:discard (:anything t))))
        ))

(setq ub/org-agenda-settings-tasks-urgent-wo-date
      `(
        (org-agenda-overriding-header "\n")
        (org-deadline-warning-days 365)
        (org-agenda-sorting-strategy '((todo todo-state-down priority-down timestamp-up)))
        (org-agenda-hide-tags-regexp (concat ub/tags-main-hide "\\|important\\|urgent"))
        (org-super-agenda-groups
         '(
           ;; (:discard (:habit t))
           ;; (:discard (:tag "recurring"))
           ;; (:discard (:tag "project_title"))
           ;; (:discard (:date t))
           ;; (:discard (:scheduled t))
           ;; (:discard (:deadline t))
           (:name "Urgent w/o Date"
            ;;:discard (:date t)
            :discard (:scheduled t)
            :discard (:deadline t)
            :discard (:tag "important")
            :and (:tag "urgent")
            :order 1)
           (:discard (:anything t))))
        ))

(setq ub/org-agenda-settings-plan
      `(
        (org-agenda-time-grid nil)
        (org-deadline-warning-days 0)
        (org-habit-show-habits-only-for-today nil) ;; NOTE will have same effect of org-agenda-show-future-repeats
        (org-habit-show-habits t)
        ;; (org-habit-show-all-today nil)
        ;; (org-scheduled-past-days 0)
        ;; (org-agenda-skip-deadline-if-done t)
        ;; (org-agenda-skip-scheduled-if-done t)
        (org-agenda-show-future-repeats t)
        (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
        (org-agenda-todo-ignore-timestamp 'past)
        (org-agenda-sorting-strategy '((agenda habit-down timestamp-up category-keep)))
        (org-super-agenda-groups
         '(
           ;;(:discard (:tag "habit"))
           ;;(:discard (:habit "t"))
           ;;(:discard (:date past))
           (:discard (:tag "j@tomorrowself"))
           (:discard (:tag "j@1todaygreat"))
           (:discard (:tag "j@schedule"))
           (:discard (:tag "r@daily"))
           (:discard (:tag "r@batch"))
           (:discard (:tag "h@daily"))
           (:discard (:tag "h@batch"))
           (:discard (:todo "T@HOLD"))
           ;; (:discard (:and (:deadline past :not (:tag "recurring"))))
           ;; (:discard (:and (:scheduled past :not (:tag "recurring"))))
           ;; NOTE if you discard habits aand recurrings, it will also discard future repeats
           (:discard (:and (:deadline past :not (:tag "habit" :tag "recurring"))))
           (:discard (:and (:scheduled past :not (:tag "habit" :tag "recurring"))))
           ;; (:discard (:deadline past))
           ;; (:discard (:scheduled past))
           ;;(:discard (:todo "KILL"))
           (:name ""
            :anything t)))
        ))

(setq ub/org-agenda-settings-tasks-all-schedule-deadline-type
      `(
        (org-agenda-format-date "")
        (org-agenda-span 'day)
        (org-agenda-time-grid nil)
        (org-deadline-warning-days 365)
        (org-agenda-sorting-strategy '((agenda alpha-up timestamp-up)))
        (org-super-agenda-groups
         '(
           (:discard (:habit t))
           (:discard (:tag "recurring"))
           ;;(:discard (:tag "project_title"))
           (:discard (:not (:scheduled t :deadline t))) ; discard timestamps without scheduled and deadline
           (:name "Past Deadlines - Shit!"
            :and (:deadline past :tag "important" :tag "urgent")
            :and (:deadline past :tag "important")
            :and (:deadline past :tag "urgent")
            :deadline past
            :order 1)
           (:name "Active Future Deadlines - Should Be Working On!" ; should worry about deadline because you should be working on it
            :time-grid t
            :and (:scheduled past :deadline future :tag "important" :tag "urgent")
            :and (:scheduled past :deadline future :tag "important")
            :and (:scheduled past :deadline future :tag "urgent")
            :and (:scheduled past :deadline future)
            :order 1)
           (:name "Ambiguous Future Deadlines - Haven't Scheduled Yet!" ; should worry about deadline because you should be working on it
            :time-grid t
            :and (:scheduled nil :deadline future :tag "important" :tag "urgent")
            :and (:scheduled nil :deadline future :tag "important")
            :and (:scheduled nil :deadline future :tag "urgent")
            :and (:scheduled nil :deadline future)
            :order 2)
           (:name "Chill Future Deadlines - Still Have Time :\)" ; don't worry about deadline because you still have time to start since it is scheduled in the future
            :time-grid t
            :and (:scheduled future :deadline future :tag "important" :tag "urgent")
            :and (:scheduled future :deadline future :tag "important")
            :and (:scheduled future :deadline future :tag "urgent")
            :and (:scheduled future :deadline future)
            :order 3)
           (:name "Waiting w/o Deadline - Waiting for Forever?"
            :time-grid t
            :and (:todo "T@WAITING" :deadline nil :tag "important" :tag "urgent")
            :and (:todo "T@WAITING" :deadline nil :tag "important")
            :and (:todo "T@WAITING" :deadline nil :tag "urgent")
            :and (:todo "T@WAITING" :deadline nil)
            :order 6)
           (:name "In-Progress w/o Deadline - Working on Forever?"
            :time-grid t
            :and (:todo "T@IN-PROGRESS" :deadline nil :tag "important" :tag "urgent")
            :and (:todo "T@IN-PROGRESS" :deadline nil :tag "important")
            :and (:todo "T@IN-PROGRESS" :deadline nil :tag "urgent")
            :and (:todo "T@IN-PROGRESS" :deadline nil)
            :order 7)
           (:name "Hold w/o Deadline - Working on Forever?"
            :time-grid t
            :and (:todo "T@HOLD" :deadline nil :tag "important" :tag "urgent")
            :and (:todo "T@HOLD" :deadline nil :tag "important")
            :and (:todo "T@HOLD" :deadline nil :tag "urgent")
            :and (:todo "T@HOLD" :deadline nil)
            :order 8)
           (:name "Self-deceptiving :S - Tasks w/o deadline" ;scheduled it already but haven't take actions yet because you haven't set deadline !
            :time-grid t
            :and (:scheduled past :deadline nil :not (:todo "I@CONNECTED") :not (:todo "T@IN-PROGRESS") :not (:todo "T@HOLD") :not (:todo "J@IN-PROGRESS") :not (:todo "J@HOLD"))
            :order 9)
           (:name "Procrastinating Knowingly :P - Tasks w/ Deadline" ;scheduled it already but haven't take actions yet although you set deadline!
            :time-grid t
            :and (:scheduled past :deadline future :todo "TODO")
            :and (:scheduled past :deadline future :todo "T@NEXT")
            :and (:scheduled past :deadline future :todo "PROJECT")
            :and (:scheduled past :deadline future :todo "P@NEXT")
            :order 10)
           (:discard (:anything t))
           ))
        ))

(setq ub/org-agenda-settings-tasks-deadline-type
      `(
        (org-agenda-overriding-header ":ghost: Attention Beggars")
        (org-agenda-format-date "")
        (org-agenda-span 'day)
        (org-agenda-time-grid nil)
        (org-deadline-warning-days 365)
        (org-agenda-sorting-strategy '((agenda alpha-up timestamp-up)))
        (org-super-agenda-groups
         '(
           ;; (:discard (:and (:tag "habit" :not (:tag "important" :tag "urgent"))))
           ;; (:discard (:and (:tag "recurring" :not (:tag "important" :tag "urgent"))))
           ;;(:discard (:tag "project_title"))
           (:discard (:not (:scheduled t :deadline t))) ; discard timestamps without scheduled and deadline

           (:discard (:tag "habit"))

           ;;(:discard (:and (:tag "pinned" :tag "habit")))
           ;; NOTE if you have important habits, then tag pinned
           ;; (:name "Procrastination Alert :S - Habits" ; don't worry about deadline because you still have time to start since it is scheduled in the future
           ;;  :and (:scheduled past :tag "habit" :tag "important" :tag "urgent")
           ;;  :and (:scheduled past :tag "habit" :tag "important")
           ;;  :and (:scheduled past :tag "habit" :tag "urgent")
           ;;  :and (:scheduled past :tag "habit")
           ;;  :order 6)

           (:name "Important &/ Urgent Recurring Tasks" ; don't worry about deadline because you still have time to start since it is scheduled in the future
            :and (:scheduled past :tag "recurring" :tag "important" :tag "urgent")
            :and (:scheduled past :tag "recurring" :tag "important")
            :and (:scheduled past :tag "recurring" :tag "urgent")
            ;; NOTE show only important &/ urgent
            ;; :and (:scheduled past :tag "recurring")
            :order 7)

           (:name "Past Deadlines - Shit!"
            :and (:deadline past :tag "important" :tag "urgent")
            :and (:deadline past :tag "important")
            :and (:deadline past :tag "urgent")
            :deadline past
            :order 1)
           (:name "Active Future Deadlines - Should Be Working On!" ; should worry about deadline because you should be working on it
            :and (:scheduled past :deadline future :tag "important" :tag "urgent")
            :and (:scheduled past :deadline future :tag "important")
            :and (:scheduled past :deadline future :tag "urgent")
            :and (:scheduled past :deadline future)
            :order 2)
           (:name "Ambiguous Future Deadlines - Haven't Scheduled Yet!" ; should worry about deadline because you should be working on it
            :and (:scheduled nil :deadline future :tag "important" :tag "urgent")
            :and (:scheduled nil :deadline future :tag "important")
            :and (:scheduled nil :deadline future :tag "urgent")
            :and (:scheduled nil :deadline future)
            :order 3)
           ;; (:name "Chill Future Deadlines - Still Have Time :\)" ; don't worry about deadline because you still have time to start since it is scheduled in the future
           ;;  :and (:scheduled future :deadline future :tag "important" :tag "urgent")
           ;;  :and (:scheduled future :deadline future :tag "important")
           ;;  :and (:scheduled future :deadline future :tag "urgent")
           ;;  :and (:scheduled future :deadline future)
           ;;  :order 4)
           (:name "Important &/ Urgent Tasks w/o Deadline" ; don't worry about deadline because you still have time to start since it is scheduled in the future
            :and (:scheduled past :deadline nil :tag "important" :tag "urgent")
            :and (:scheduled past :deadline nil :tag "important")
            :and (:scheduled past :deadline nil :tag "urgent")
            :order 5)

           (:discard (:anything t))))
        ))

(setq ub/org-agenda-settings-tasks-forgot-to-schedule-or-deadline
      `(
        (org-super-agenda-groups
         '(
           (:discard (:tag "habit")) ; warning it doesn't include h@.+ :/
           (:discard (:and (:deadline t)))
           (:discard (:and (:scheduled t)))
           (:discard (:tag "project_title"))
           (:discard (:tag "recurring"))
           (:name "Started but Forgot to Schedule/Deadline :/ - Commit Properly!"
            :and (:todo "T@IN-PROGRESS" :tag "important" :tag "urgent")
            :and (:todo "T@IN-PROGRESS" :tag "important")
            :and (:todo "T@IN-PROGRESS" :tag "urgent")
            :and (:todo "T@WAITING" :tag "important" :tag "urgent")
            :and (:todo "T@WAITING" :tag "important")
            :and (:todo "T@WAITING" :tag "urgent")
            :and (:todo "T@HOLD" :tag "important" :tag "urgent")
            :and (:todo "T@HOLD" :tag "important")
            :and (:todo "T@HOLD" :tag "urgent")
            :todo "T@IN-PROGRESS"
            :todo "T@WAITING"
            :todo "T@HOLD"
            :order 10)
           (:discard (:anything t))))
        ))

(setq ub/org-agenda-settings-project-roadmap
      `(
        (org-agenda-format-date "")
        (org-agenda-span 'day)
        (org-agenda-time-grid nil)
        (org-deadline-warning-days 365)
        (org-agenda-skip-scheduled-if-deadline-is-shown t)
        (org-super-agenda-groups
         '((:auto-planning t)))
        ))

(setq ub/org-agenda-settings-project-tasks-outline-path
      `(
        (org-super-agenda-groups
         '(
           ;;(:discard (:todo "LOG"))
           ;;(:discard (:and (:tag "project" :not (:tag "project_title") :todo "J@HOLD")))
           ;;(:discard (:and (:tag "project" :todo "J@HOLD")))
           ;;(:auto-category t)
           (:auto-outline-path t)
           ))
        ))

;; empty custom agenda list
(setq org-agenda-custom-commands
      `(
        ))

(add-to-list 'org-agenda-custom-commands
             `("d" . "Today")
             t)

(add-to-list 'org-agenda-custom-commands
             `("d1" "Plan"
               (
                (tags "focus_task"
                      ,(append ub/org-agenda-files-all-code-block
                               `((org-agenda-hide-tags-regexp (concat ub/tags-main-hide "\\|focus_task"))
                                 (org-agenda-prefix-format (quote ((tags . "%i %-25:c"))))
                                 (org-agenda-overriding-header ":dart: Focus Task")
                                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                                 )))

              (tags "j@1todaygreat"
                      ((org-agenda-files '(,ub/journal-current-file))
                       (org-agenda-hide-tags-regexp (concat ub/tags-main-hide "\\|j@1todaygreat"))
                       (org-agenda-prefix-format (quote ((tags . "%i %-25:c"))))
                       (org-agenda-overriding-header ":rocket: 1 Thing That Would Make Today Great")
                       (org-super-agenda-groups
                              '((:name "" :date today)
                                (:discard (:anything t))))
                       ))

              (tags "j@tomorrowself"
                      ((org-agenda-files '(,ub/journal-current-file))
                       (org-agenda-hide-tags-regexp (concat ub/tags-main-hide "\\|j@tomorrowself"))
                       (org-agenda-prefix-format (quote ((tags . "%i %-25:c"))))
                       (org-agenda-overriding-header ":older_man_tone5: 1 Thing That Would Advice Tomorrow-Self")
                       (org-super-agenda-groups
                              '((:name "" :date today)
                                (:discard (:anything t))))
                       ))


                (agenda "" ,(append ub/org-agenda-files-all-code-block ub/org-agenda-settings-todays-schedule
                                    `((org-agenda-prefix-format (quote ((agenda . " %i %-25:c%-12t% s")))))
                                    ))
                (agenda "" ,(append ub/org-agenda-files-all-code-block ub/org-agenda-settings-habit-pinned
                                    `((org-agenda-prefix-format (quote ((agenda . " %i %-12:c")))
                                                                ))))

                (agenda "" ,(append ub/org-agenda-files-all-code-block ub/org-agenda-settings-tasks-deadline-type
                                    `((org-agenda-prefix-format (quote ((agenda . " %i %-25:c%-t% s")))
                                                                ))
                                    ))

                (agenda ""
                      ((org-agenda-files ub/org-agenda-files-all)
                       (org-agenda-hide-tags-regexp (concat ub/tags-main-hide "\\|reminder"))
                       (org-agenda-prefix-format (quote ((agenda . "%i %-25:c"))))
                       (org-agenda-span 1)
                       (org-agenda-format-date "")
                       (org-agenda-show-future-repeats t)
                       (org-agenda-overriding-header "🎗 Reminders")
                       (org-super-agenda-groups
                        '((:name "Today" :order 5 :tag "reminder")
                          (:discard (:anything t))))
                       ))

                (tags "+pinned+reminder"
                      ((org-agenda-files '(,ub/gtd-horizon-file))
                       (org-agenda-hide-tags-regexp (concat ub/tags-main-hide "\\|reminder\\|pinned"))
                       (org-agenda-prefix-format (quote ((tags . "%i %-25:c"))))
                       (org-agenda-span 1)
                       (org-agenda-format-date "")
                       (org-agenda-show-future-repeats t)
                       (org-agenda-block-separator nil)
                       (org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '((:name "Pinned" :anything t)
                          ;;(:discard (:anything t))
                          ))
                       ))

                ;; (tags "g@phd"
                ;;       ((org-agenda-files '(,ub/phd-horizon-file))
                ;;        (org-agenda-hide-tags-regexp (concat ub/tags-main-hide "\\|g@phd"))
                ;;        (org-agenda-overriding-header ":nerd_face: PhD Goals")))

                ))
             t)

(add-to-list 'org-agenda-custom-commands
             `("d2" "Review"
               (
                (agenda "" ,(append ub/org-agenda-settings-report
                                    `(
                                      (org-agenda-files (append ub/gtd-main-files-list
                                                                ub/journal-main-files-list))
                                      (org-agenda-sorting-strategy '((time-up)))
                                      (org-agenda-overriding-header ":timer_clock: Clocked Today")
                                      (org-agenda-span 'day)
                                      (org-agenda-prefix-format (quote ((agenda . " %i %-25:c%-12t% s"))))
                                      (org-agenda-format-date "")
                                      )))

                (alltodo "" (
                             ;; (org-agenda-files `(,ub/gtd-pocket-file
                             ;;                     ,ub/gtd-refile-file))
                             (org-agenda-files (append `(
                                                         ,ub/gtd-phone-file
                                                         ,ub/gtd-refile-file)
                                                       ub/journal-main-files-list))
                             (org-agenda-overriding-header ":notebook: Refile")
                             (org-agenda-sorting-strategy '((todo todo-state-down priority-down timestamp-up)))
                             (org-agenda-prefix-format (quote ((todo . " %i %-12:c"))))
                             ;;(org-agenda-sorting-strategy '((todo time-up)))  ; BUG NOTE: doesn't work somehow ignored :/
                             (org-super-agenda-groups
                              '((:auto-ts-reverse t)))
                             ))

                ;; (org-ql-block `(ts :from ,(ts-format "%Y-%m-%d 09:00") :to ,(ts-format "%Y-%m-%d 03:00" (ts-adjust 'day 1 (ts-now))))
                ;;               ((org-ql-block-header ":turkey: Today's Recent Items"))
                ;;               ;; https://www.reddit.com/r/orgmode/comments/fvbnps/trying_and_failing_to_get_a_custom_orgagenda_view/fmz8ayi/
                ;;               ;;:sort 'date ;; NOTE doesn't work
                ;;               ;;:super-groups '((:auto-ts t)) ;; NOTE doesn't work
                ;;               )

                ;; TODO clocked items including after midnight
                ;; (org-ql-block `(clocked :from ,(ts-format "%Y-%m-%d 09:00" (ts-adjust 'day -1 (ts-now))) :to ,(ts-format "%Y-%m-%d 03:00" (ts-adjust 'day 0 (ts-now))))
                ;;               ((org-ql-block-header ":turkey: Clocked"))
                ;;               ;; https://www.reddit.com/r/orgmode/comments/fvbnps/trying_and_failing_to_get_a_custom_orgagenda_view/fmz8ayi/
                ;;               ;;:sort 'date ;; NOTE doesn't work
                ;;               ;;:super-groups '((:auto-ts t)) ;; NOTE doesn't work
                ;;               )

                ))
             t)

(add-to-list 'org-agenda-custom-commands
             `("w" . "Weekly/Longer")
             t)

(add-to-list 'org-agenda-custom-commands
             `("wp" . "Plan")
             t)

(add-to-list 'org-agenda-custom-commands
             `("wp1" "Important &/ Urgent - Schedules &/ Deadlines"
               (
                (agenda "" ,(append ub/org-agenda-files-all-code-block ub/org-agenda-settings-tasks-all-schedule-deadline-type
                                    `(
                                      (org-agenda-overriding-header "")
                                      (org-agenda-prefix-format (quote ((agenda . " %i %-25:c%-13s"))))
                                      (org-agenda-block-separator nil)
                                      )))

                (alltodo "" ,(append ub/org-agenda-files-all-code-block
                                     ub/org-agenda-settings-tasks-important-urgent-wo-date
                                     `((org-agenda-block-separator nil)
                                       (org-agenda-prefix-format (quote ((todo . " %i %-25:c"))))
                                       )))

                (alltodo "" ,(append ub/org-agenda-files-all-code-block
                                     ub/org-agenda-settings-tasks-important-wo-date
                                     `((org-agenda-block-separator nil)
                                       (org-agenda-prefix-format (quote ((todo . " %i %-25:c"))))
                                       )))

                (alltodo "" ,(append ub/org-agenda-files-all-code-block
                                     ub/org-agenda-settings-tasks-urgent-wo-date
                                     `((org-agenda-block-separator nil)
                                       (org-agenda-prefix-format (quote ((todo . " %i %-25:c"))))
                                       )))

                (alltodo "" ,(append ub/org-agenda-files-all-code-block
                                     ub/org-agenda-settings-tasks-forgot-to-schedule-or-deadline
                     `(
                       (org-agenda-overriding-header "")
                       (org-agenda-block-separator nil)
                       (org-agenda-prefix-format (quote ((todo . " %i %-25:c"))))
                       )))

                ))
             t)

(add-to-list 'org-agenda-custom-commands
             `("wp2" "This Week"
               (
                ;; NOTE no need to show them in a separated place as org agenda shows them in the current day and filtering past tasks cause them to be filtered out from the future as well...
                (agenda "" ,(append ub/org-agenda-files-all-code-block ub/org-agenda-settings-plan
                                    `(
                                      (org-agenda-start-on-weekday nil)
                                      (org-agenda-span (ub/days-to-next-sunday))
                                      ;;(org-agenda-span 'week)
                                      (org-agenda-overriding-header "")
                                      (org-agenda-prefix-format (quote ((agenda . " %i %-25:c%-12t% s"))))
                                      )))
                ))
             t)

(add-to-list 'org-agenda-custom-commands
             `("wr" . "Review")
             t)

(add-to-list 'org-agenda-custom-commands
             `("wrc" . "Clocked")
             t)

(add-to-list 'org-agenda-custom-commands
             `("wrca" "All"
               agenda ""
               ;; agenda settings
               ,(append
                 ub/org-agenda-files-all-code-block ub/org-agenda-settings-report
                 '((org-agenda-span 'week)
                   (org-agenda-start-on-weekday 1)
                   (org-agenda-overriding-header ":timer_clock: Clocked Review"))
                 )
               )
             t)


(add-to-list 'org-agenda-custom-commands
             `("wrcw" "Work"
               agenda ""
               ;; agenda settings
               ,(append ub/org-agenda-settings-report
                 '((org-agenda-files ub/gtd-work-main-files-list)
                   (org-agenda-span 'week)
                   (org-agenda-start-on-weekday 1)
                   (org-agenda-overriding-header ":timer_clock: Clocked Review"))
                 )
               )
             t)

(add-to-list 'org-agenda-custom-commands
             `("wrcp" "Personal"
               agenda ""
               ;; agenda settings
               ,(append ub/org-agenda-settings-report
                 `((org-agenda-files (append ub/gtd-personal-main-files-list
                                              ub/journal-main-files-list))
                   (org-agenda-span 'week)
                   (org-agenda-start-on-weekday 1)
                   (org-agenda-overriding-header ":timer_clock: Clocked Review"))
                 )
               )
             t)

(add-to-list 'org-agenda-custom-commands
             `("wrr" "Refile"
               (
                (alltodo "" (
                             (org-agenda-files (append `(
                                                         ,ub/gtd-phone-file
                                                         ,ub/gtd-refile-file)
                                                       ub/journal-main-files-list
                                                       ;;ub/zk-refile-files-list
                                                       ))
                             (org-agenda-overriding-header ":notebook: Refile")
                             (org-agenda-prefix-format (quote ((todo . " "))))
                             (org-agenda-sorting-strategy '((todo todo-state-down priority-down timestamp-up)))
                             ;;(org-agenda-sorting-strategy '((todo time-up)))  ; BUG NOTE: doesn't work somehow ignored :/
                             (org-super-agenda-groups
                              '((:auto-ts-reverse t)))
                             ))
                ))
             t)

(add-to-list 'org-agenda-custom-commands
             `("u" . "Utility")
             t)

(add-to-list 'org-agenda-custom-commands
             `("ue" "Recent"
               (
                (org-ql-block `(ts :from ,(ts-format "%Y-%m-%d 09:00") :to ,(ts-format "%Y-%m-%d 03:00" (ts-adjust 'day 1 (ts-now))))
                              ((org-ql-block-header ":turkey: Today's Recent Items"))
                              ;; https://www.reddit.com/r/orgmode/comments/fvbnps/trying_and_failing_to_get_a_custom_orgagenda_view/fmz8ayi/
                              ;;:sort 'date ;; NOTE doesn't work
                              ;;:super-groups '((:auto-ts t)) ;; NOTE doesn't work
                              )

                ;; TODO clocked items including after midnight
                ;; (org-ql-block `(clocked :from ,(ts-format "%Y-%m-%d 09:00" (ts-adjust 'day -1 (ts-now))) :to ,(ts-format "%Y-%m-%d 03:00" (ts-adjust 'day 0 (ts-now))))
                ;;               ((org-ql-block-header ":turkey: Clocked"))
                ;;               ;; https://www.reddit.com/r/orgmode/comments/fvbnps/trying_and_failing_to_get_a_custom_orgagenda_view/fmz8ayi/
                ;;               ;;:sort 'date ;; NOTE doesn't work
                ;;               ;;:super-groups '((:auto-ts t)) ;; NOTE doesn't work
                ;;               )

                ))
             t)

(add-to-list 'org-agenda-custom-commands
             `("uz" "ZK"
               (
                (alltodo "" (
                             (org-agenda-files (append `(;;,ub/gtd-pocket-file
                                                         ;; ,ub/gtd-refile-file)
                                                         )
                                                         ;;ub/journal-main-files-list
                                                         ub/zk-refile-files-list
                                                         ))
                                               (org-agenda-overriding-header ":notebook: Zettelkasten")
                                               (org-agenda-prefix-format (quote ((todo . " "))))
                                               (org-agenda-sorting-strategy '((todo todo-state-down priority-down timestamp-up)))
                                               ;;(org-agenda-sorting-strategy '((todo time-up)))  ; BUG NOTE: doesn't work somehow ignored :/
                                               (org-super-agenda-groups
                                                '((:auto-ts-reverse t)))
                                               ))
                         ))
               t)

(add-to-list 'org-agenda-custom-commands
             `("uj" "OP Project"
               (

(agenda "" ,(append ub/org-agenda-settings-project-roadmap
                    `(
                      (org-agenda-files `(,ub/project-object-permanence-file))
                      (org-agenda-overriding-header "Roadmap")
                      ))
        )

(alltodo "" ,(append ub/org-agenda-settings-project-tasks-outline-path
                     `(
                       (org-agenda-files `(,ub/project-object-permanence-file))
                       (org-agenda-overriding-header "Tasks")
                       ))
         )

)) ;; end of
t)

(defun ub/org-agenda-plan (&optional arg)
  (interactive)
  (org-agenda arg "wp2")
  (split-window-right)
  (org-agenda arg "wp1")
  (org-agenda-redo)
  (beginning-of-buffer)
  (other-window 1)
  (org-agenda-redo)
  (beginning-of-buffer)
  (org-gcal-sync)
  )
(map! "C-c u p" #'ub/org-agenda-plan)

(defun ub/org-agenda-today (&optional arg)
  (interactive)
  (org-agenda arg "d2")
  (split-window-right)
  (org-agenda arg "d1")
  (org-agenda-redo)
  (beginning-of-buffer)
  (other-window 1)
  (org-agenda-redo)
  (beginning-of-buffer)
  (org-gcal-sync)
  )
(map! "C-c u d" #'ub/org-agenda-today)

(require 'elgantt)
(setq ub/elgantt-files (append ub/gtd-me-projects-main-files-list ;  is a list
                               ub/gtd-work-projects-main-files-list ; is a list
                               ))
;; (add-to-list 'ub/elgantt-files ub/personal-tasks-file)
;; (add-to-list 'ub/elgantt-files ub/phd-tasks-file)
;; (add-to-list 'ub/elgantt-files ub/proaut-tasks-file)
;; (add-to-list 'ub/elgantt-files ub/personal-horizon-file)
;; (add-to-list 'ub/elgantt-files ub/phd-horizon-file)
;; (add-to-list 'ub/elgantt-files ub/proaut-horizon-file)

(setq elgantt-agenda-files ub/elgantt-files)
;;(setq elgantt-start-date "2021-05-01")
(setq elgantt-header-type 'outline)
(setq elgantt-insert-blank-line-between-top-level-header t)
(setq elgantt-startup-folded nil)
(setq elgantt-show-header-depth t)
(setq elgantt-draw-overarching-headers t)

(defun ub/elgantt-personal-open ()
  "Open gantt personal"
  (interactive)
  (switch-to-buffer "*El Gantt Personal*")
  ;; (setq ub/elgantt-personal-files (append ub/gtd-me-projects-main-files-list ;  is a list
  ;;                                         ;;ub/gtd-me-areas-files-list ; is a list
  ;;                                         ))
  ;; (add-to-list 'ub/elgantt-personal-files ub/gtd-me-tasks-file)
  ;; (add-to-list 'ub/elgantt-personal-files ub/gtd-me-horizon-file)
  (setq elgantt-agenda-files ub/gtd-me-projects-main-files-list)
  (elgantt-mode))

(defun ub/elgantt-phd-open ()
  "Open gantt phd"
  (interactive)
  (switch-to-buffer "*El Gantt PhD*")
  ;; (setq ub/elgantt-phd-files (append ub/phd-projects-main-files-list ;  is a list
  ;;                                    ub/phd-areas-files-list ; is a list
  ;;                                    ))
  ;; (add-to-list 'ub/elgantt-phd-files ub/phd-tasks-file)
  ;; (add-to-list 'ub/elgantt-phd-files ub/phd-horizon-file)
  (setq elgantt-agenda-files ub/gtd-work-projects-main-files-list)
  (elgantt-mode))


(defun ub/elgantt-op-project-open ()
  "Open gantt phd"
  (interactive)
  (switch-to-buffer "*El Gantt OP Project*")
  (setq elgantt-agenda-files ub/project-object-permanence-file)
  (elgantt-mode))

;; taken from original repo
(setq elgantt-user-set-color-priority-counter 0)
(elgantt-create-display-rule draw-scheduled-to-deadline
  :parser ((elgantt-color . ((when-let ((colors (org-entry-get (point) "ELGANTT-COLOR")))
                               (s-split " " colors)))))
  :args (elgantt-scheduled elgantt-color elgantt-org-id)
  :body ((when elgantt-scheduled
           (let ((point1 (point))
                 (point2 (save-excursion
                           (elgantt--goto-date elgantt-scheduled)
                           (point)))
                 (color1 (or (car elgantt-color)
                             "black"))
                 (color2 (or (cadr elgantt-color)
                             "red")))
             (when (/= point1 point2)
               (elgantt--draw-gradient
                color1
                color2
         \       (if (< point1 point2) point1 point2) ;; Since cells are not necessarily linked in
                (if (< point1 point2) point2 point1) ;; chronological order, make sure they are sorted
                nil
                `(priority ,(setq elgantt-user-set-color-priority-counter
                                  (1- elgantt-user-set-color-priority-counter))
                           ;; Decrease the priority so that earlier entries take
                           ;; precedence over later ones (note: it doesn’t matter if the number is negative)
                           :elgantt-user-overlay ,elgantt-org-id)))))))

(use-package! org-clock-budget
  :config
  (setq org-clock-budget-daily-budgetable-hours 12))
