;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ugur Bolat"
      user-mail-address "blabla@bla.com")

;; theme
(require 'modus-themes)
(load-theme 'modus-vivendi t)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.

(setq display-line-numbers-type nil)

;; wraps texts beyond window width
;; NOTE doom already enables it
;;(global-visual-line-mode t)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(custom-set-faces
 ;; make current-time-string now visible
 '(org-agenda-current-time ((t (:inherit org-time-grid :foreground "#eb6c63")))))

(setq emojify-emoji-set "twemoji-v2")

;; NOTE until finding alternative for visualizing the position similar minimap that is more minimal
(setq scroll-bar-mode 'right)
(scroll-bar-mode)

(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "<f5>") 'revert-buffer)

;; disable annoying beeping sound
(setq visible-bell 1)

;; Auto-refresh dired on file change
;;(add-hook 'dired-mode-hook 'auto-revert-mode)

;;(setq menu-bar-mode t) ;; yes i like menubars!
(global-set-key (kbd "s-<right>") 'move-end-of-line)
(global-set-key (kbd "M-Q") 'unfill-region)
(global-set-key (kbd "M-W") 'unfill-paragraph)
(require 'w32-browser)

(global-auto-revert-mode t)
(defun message-buffer-goto-end-of-buffer (&rest args)
  (let* ((win (get-buffer-window "*Messages*"))
         (buf (and win (window-buffer win))))
    (and win (not (equal (current-buffer) buf))
         (set-window-point
          win (with-current-buffer buf (point-max))))))

(advice-add 'message :after 'message-buffer-goto-end-of-buffer)
;;(auto-revert-tail-mode t) ; BUG
(setq auto-revert-use-notify t)

;; prompts for unsafe variables for the first time
(setq enable-local-variables 't)

(add-hook 'writeroom-mode-disable-hook
          (lambda () (display-line-numbers-mode nil)))
(add-hook 'writeroom-mode-enable-hook
          (lambda () (display-line-numbers-mode -1)))
(add-hook 'writeroom-mode-enable-hook
          (lambda () (setq line-spacing 10)))
(add-hook 'writeroom-mode-disable-hook
          (lambda () (setq line-spacing nil)))

(setq projectile-indexing-method 'alien)
(setq projectile-enable-caching t)
(setq projectile-project-search-path `(,ub/gtd-root-dir
                                       ;; add more later here
                                       ))
(add-to-list 'projectile-globally-ignored-directories "0_archive")
(add-to-list 'projectile-globally-ignored-directories ".local")
(add-to-list 'projectile-globally-ignored-file-suffixes ".org_archive")

(projectile-global-mode)

(use-package! treemacs-projectile
  :after treemacs projectile
  )
(use-package! treemacs
  :bind* (("C-x C-n" . treemacs))
  :config
  (setq treemacs-is-never-other-window nil)
  )

(use-package! centaur-tabs
  :config
  (centaur-tabs-mode -1)
  (centaur-tabs-group-by-projectile-project)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))

(use-package! pdf-continuous-scroll-mode
  :config
  (require 'pdf-continuous-scroll-mode)
  (add-hook 'pdf-view-mode-hook 'pdf-continuous-scroll-mode)
  (add-hook 'pdf-view-mode-hook 'pdf-cscroll-toggle-mode-line))

(global-activity-watch-mode)

(defun ub/archive-default ()
  "Archives the current subtree default which datetree format. Useful for project tasks."
  (interactive)
  (with-current-buffer (current-buffer)
    (when (eq major-mode 'org-agenda-mode)
      (org-agenda-goto)
      (setq am-i-in-org-agenda 1))
    (let* ((file-window (selected-window))
           (buffname (replace-regexp-in-string " " "_" (downcase (file-name-base buffer-file-name))))
           (dirname (concat buffname ".assets/archive/"))
           (filename (concat dirname buffname ".org_archive"))
           (org-archive-location (concat filename "::")))
      (make-directory dirname t)
      (org-archive-subtree-hierarchical)
      (when (eq am-i-in-org-agenda 1)
        (progn
          (delete-window file-window)
          (org-agenda-redo))))))

(map! "C-c u a a" #'ub/archive-default)

(defun ub/archive-selection(&optional done-or-kill)
  ;; Default is KILL
  (interactive)
  ;; NOTE couln't make org-agenda-get-todos work, instead goto heading
  (with-current-buffer (current-buffer)
    (when (eq major-mode 'org-agenda-mode)
      (org-agenda-goto)
      (setq am-i-in-org-agenda 1))
    (let ((file-window (selected-window)))
      (if done-or-kill
          (when (not (string-match-p "DONE" (org-get-todo-state)))
            (org-todo 'done))
        (when (not (string-match-p "KILL" (org-get-todo-state)))
          (org-todo "KILL"))
        )
      (let* ((org-agenda-archive-file (ido-completing-read "Selected archive file: " ub/gtd-assets-dir-name-list))
                                        ;(org-agenda-archive-location (format "%s/%s.assets/%s.org_archive::* %s" ub/gtd-root-dir org-agenda-archive-file org-agenda-archive-file (format-time-string "%Y-%m-%d %A"))))
             (org-archive-location (format "%s/%s.assets/archive/%s.org_archive::" ub/gtd-root-dir org-agenda-archive-file org-agenda-archive-file)))
        (org-archive-subtree-hierarchical))
      (when (eq am-i-in-org-agenda 1)
        (progn
          (delete-window file-window)
          (org-agenda-redo))))
 ))

(map! "C-c u a s" #'ub/archive-selection)

;; TODO might be useful to catch if C-g pressed. if so restore tag lists
;; https://stackoverflow.com/questions/884498/how-do-i-intercept-ctrl-g-in-emacs

;; Tagging automatically from tag list based on the content of the heading
;; https://stackoverflow.com/questions/29788639/automatically-assigning-tags-in-org-mode
(defun ub/org-agenda-auto-tag (tag-list)
  (interactive)
  (save-excursion)
  (let ((this-bufffer (current-buffer)))
    (org-agenda-goto)
    (let ((alltags (append org-tag-persistent-alist org-current-tag-alist tag-list))
          (headline-words (split-string (downcase (org-get-heading t t)))))
      (mapcar (lambda (word) (if (assoc word alltags)
                                 (org-toggle-tag word 'on)))
              headline-words))
    (+workspace/close-window-or-workspace)
    (switch-to-buffer this-buffer)))

(defun ub/org-agenda-refile (file headline &optional arg)
  "Refiles with tag completion"

  (interactive)
  (save-excursion)
  ;; retrieving tag list from refile file for tag prompt and completion
  (let ((this-buffer (current-buffer))
        (this-tag-alist org-current-tag-alist))
    (find-file file)
    (let ((learning-group-tag-alist (append (org-get-buffer-tags) org-current-tag-alist)))
      (switch-to-buffer this-buffer)
      (setq org-current-tag-alist learning-group-tag-alist)
      (ub/org-agenda-auto-tag learning-group-tag-alist)
      (counsel-org-tag))
    (setq org-current-tag-alist this-tag-alist))

  (let ((agenda-buffer (current-buffer))
        (agenda-point (point)))
    (let ((pos (save-excursion
                 (find-file file)
                 (org-find-exact-headline-in-buffer headline))))
      (org-agenda-refile nil (list headline file nil pos)))
    (switch-to-buffer agenda-buffer)
    (goto-char agenda-point)))


(defun ub/refile-zk (note-directory)
  "Refiles to org-roam notes"
  ;; (interactive)
  ;; (set-buffer-modified-p t)
  ;;(hydra-keyboard-quit)

  ;; (cond ((equal note-directory "refile")
  ;;        (progn (setq org-roam-directory ub/zk-refile-dir)
  ;;               (setq org-roam-db-location (expand-file-name ".refile-roam.db" ub/zk-refile-dir))))
  ;;       ((equal note-directory "private")
  ;;        (progn (setq org-roam-directory ub/zk-private-dir)
  ;;               (setq org-roam-db-location (expand-file-name ".private-roam.db" ub/zk-private-dir))))
  ;;       ((equal note-directory "shared")
  ;;        (progn (setq org-roam-directory ub/zk-shared-dir)
  ;;               (setq org-roam-db-location (expand-file-name ".shared-roam.db" ub/zk-shared-dir)))))
  ;;(let ((agenda-buffer (current-buffer)))
  ;;   (save-excursion)
  ;;(with-temp-buffer
  (with-current-buffer (current-buffer)
    (interactive)
    (let ((am-i-in-org-agenda 0))
      (when (eq major-mode 'org-agenda-mode)
        (org-agenda-goto)
        (setq am-i-in-org-agenda 1))
      (let ((file-window (selected-window)))
        ;; update TODO state
        ;; NOTE didn't work inside capture template
        (cond ((equal note-directory "refile")
               (progn (org-todo "N@NEXT")
                      ))
              ((equal note-directory "private")
               (progn (org-todo "N@DONE")
                      ))
              ((equal note-directory "shared")
               (progn (org-todo "N@DONE")
                      )))
        (setq note-heading (org-get-heading t t t t))
        (setq org-roam-capture--info
              `((title . ,note-heading)
                ;;(ref . "aasd2")
                (slug . ,(funcall org-roam-title-to-slug-function note-heading))
                ))
        ;;(org-roam-capture--fill-template "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n#+ROAM_KEY: ${ref}\n#+ROAM_TAGS: literature\n#+OPTIONS: toc:nil\n\n")
        (org-cut-special)
        ;; TODO saving buffer file doesn't work :/
        (save-buffer (current-buffer))
        ;;(unless org-roam-mode (org-roam-mode))
        (setq org-roam-capture--context 'capture)
        (condition-case err
            (cond ((equal note-directory "refile")
                   (progn (org-roam-capture--capture nil "r")
                          ))
                  ((equal note-directory "private")
                   (progn (org-roam-capture--capture nil "zp")
                          ))
                  ((equal note-directory "shared")
                   (progn (org-roam-capture--capture nil "zs")
                          ))
                  ((equal note-directory "phd")
                   (progn (org-roam-capture--capture nil "zh")
                          )))
          (error (user-error "%s.  Please adjust `org-roam-capture-templates'"
                             (error-message-string err))))
        (org-paste-special nil)
        ;; delete previous heading that is extra from org-roam-capture
        (org-previous-visible-heading 1)
        (org-mark-subtree)
        (delete-region (region-beginning) (region-end))
        (org-capture-finalize)
        (when (eq am-i-in-org-agenda 1)
          (progn
            (delete-window file-window)
            (org-agenda-redo)))
        )
      )))

(defun ub/refile-zk-to-refile ()
  (interactive)
  (ub/refile-zk "refile"))
(defun ub/refile-zk-to-private ()
  (interactive)
  (ub/refile-zk "private"))
(defun ub/refile-zk-to-shared ()
  (interactive)
  (ub/refile-zk "shared"))
(defun ub/refile-zk-to-phd ()
  (interactive)
  (ub/refile-zk "phd"))

(defun ub/org-agenda-archive-as-pocket-tickler (file headline &optional arg)
  "Refiles to learning file with tag completion"
  (interactive)
  (let ((this-buffer (current-buffer)))
    (save-excursion)
    (org-agenda-goto)
    (let ((todo-state (org-get-todo-state)))
      (+workspace/close-window-or-workspace)
      (switch-to-buffer this-buffer)
      (when (not (equal "POCKET" todo-state))
        (org-agenda-todo "POCKET")))
    (ub/org-agenda-refile file headline)))

(defun ub/org-agenda-do-date-timestamp ()
  (interactive)
  (let ((this-buffer (current-buffer)))
    (org-agenda-goto)
    ;;(org-back-to-heading)
    (org-end-of-meta-data t)
    ;;(forward-line)
    (newline)
    (previous-line)
    (insert (format "DO-DATE: "))
    (org-end-of-line)
    (org-time-stamp nil)
    (newline)
    (+workspace/close-window-or-workspace)
    (switch-to-buffer this-buffer)))

(defun ub/org-agenda-cut-or-delete ()
  (interactive)
  (let ((this-buffer (current-buffer)))
    (save-excursion)
    (org-agenda-goto)
    (org-cut-special)
    (+workspace/close-window-or-workspace)
    (switch-to-buffer this-buffer)
    (org-agenda-redo)))

(defun ub/org-agenda-copy ()
  (interactive)
  (let ((this-buffer (current-buffer)))
    (save-excursion)
    (org-agenda-goto)
    (org-copy-special)
    (+workspace/close-window-or-workspace)
    (switch-to-buffer this-buffer)
    (org-agenda-redo)))

(defun ub/org-agenda-yank ()
  (interactive)
  (let ((this-buffer (current-buffer)))
    (save-excursion)
    (org-agenda-goto)
    (org-paste-special nil)
    (+workspace/close-window-or-workspace)
    (switch-to-buffer this-buffer)
    (org-agenda-redo)))

(defun ub/org-agenda-rename-header ()
  "Rename the current section's header
NOTE: will replace same header strings if exist"
  (interactive)
  (let ((this-buffer (current-buffer)))
    (save-excursion)
    (org-agenda-goto)
    (setq label (substring-no-properties (org-get-heading t t t t)))
    (switch-to-buffer this-buffer)
    (+workspace/close-window-or-workspace)
    (let ((label-list (list (read-string "Header: " label))))
      (org-agenda-goto)
      (org-back-to-heading)
      (replace-string (org-get-heading t t t t) label-list)
      (switch-to-buffer this-buffer)
      (+workspace/close-window-or-workspace)
      (org-agenda-redo))))

(defun ub/org-agenda-all-refile (&optional arg)
  (setq org-refile-targets `(
                             (,ub/refile-target-all-files-list :maxlevel . 2)
                             ))
  (org-agenda-refile)
  (switch-to-buffer (current-buffer)))

(defun ub/org-agenda-todo ()
  (save-excursion)
  (org-agenda-todo))

(defun org-agenda-agenda-cts ()
  (and (eq major-mode 'org-agenda-agenda-mode)
       (let ((args (get-text-property
                    (min (1- (point-max)) (point))
                    'org-agenda-last-args)))
         (nth 2 args))))
(defhydra hydra-org-agenda-view (:hint none)
  "
                  ^Update^             ^Refile^           ^Archive^         ^Clock^
      ^^^^^^^-----------------------------------------------------------------------------
      _g_/_r_: redo     _t_: state     _f_: org-refile    _a d_: sel-done   _c i_: in
      _C-w_: cut/del  _s_: schedule  _z f_: zk/refile   _a k_: sel-kill   _c o_: out
      _M-w_: copy     _d_: deadline  _z p_: zk/private  _a a_: def-kill   _c g_: goto
      _C-y_: paste    _o_: do-date   _z s_: zk/shared                   _c s_: recent
      _T_  : tag      _R_: rename
      _S_  : save
"
  ("<up>" org-agenda-previous-line)
  ("<down>" org-agenda-next-line)
  ("<prior>" scroll-down-command)
  ("<next>" scroll-up-command)
  ("<left>" left-char)
  ("<right>" right-char)
  ("<S-up>" org-agenda-priority-up)
  ("<S-down>" org-agenda-priority-down)
  ("<M-up>" org-agenda-drag-line-backward)
  ("<M-down>" org-agenda-drag-line-forward)
  ("SPC" org-agenda-goto)
  ("C-x 0" +workspace/close-window-or-workspace)
  ("k" org-agenda-previous-line)
  ("j" org-agenda-next-line)
  ("r" org-agenda-redo)
  ("g" org-agenda-redo-all)
  ("S" org-save-all-org-buffers)
  ("C-w" ub/org-agenda-cut-or-delete)
  ("M-w" ub/org-agenda-copy)
  ("C-y" ub/org-agenda-yank)
  ("T" counsel-org-tag)
  ("c s" org-mru-clock-in)
  ("c o" org-clock-out)
  ("c g" org-clock-goto)
  ("c i" org-agenda-clock-in)
  ("a d" (ub/archive-selection t))
  ("a k" (ub/archive-selection nil))
  ("a a" ub/archive-default)
  ("z f" (ub/refile-zk "refile"))
  ("z p" (ub/refile-zk "private"))
  ("z s" (ub/refile-zk "shared"))
  ("z h" (ub/refile-zk "phd"))
  ("f" (ub/org-agenda-all-refile))
  ("t" org-agenda-todo)
  ("C-c C-c" org-ctrl-c-ctrl-c)
  ("C-c C-k" org-kill-note-or-show-branches)
  ("s" org-agenda-schedule)
  ("d" org-agenda-deadline)
  ("R" ub/org-agenda-rename-header)
  ("o" ub/org-agenda-do-date-timestamp)
  ;; TODO don't quit when clicked w/ mouse
  ;;("<down-mouse-3>" org-mouse-move-tree-start)
  ;;("<down-mouse-1>" mouse-drag-region)
  ("q" nil "quit"))

;; Recommended binding:
(require 'org-agenda)
(define-key org-agenda-mode-map "v" 'hydra-org-agenda-view/body)

(use-package! emacs-conflict)

;;https://cundy.me/post/elfeed/

(defun concatenate-authors (authors-list)
  "Given AUTHORS-LIST, list of plists; return string of all authors
concatenated."
  (mapconcat
   (lambda (author) (plist-get author :name))
   authors-list ", "))

(defun my-search-print-fn (entry)
  "Print ENTRY to the buffer."
  (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
         (title (or (elfeed-meta entry :title)
                    (elfeed-entry-title entry) ""))
         (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
         (feed (elfeed-entry-feed entry))
         (feed-title
          (when feed
            (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
         (entry-authors (concatenate-authors
                         (elfeed-meta entry :authors)))
         (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
         (tags-str (mapconcat
                    (lambda (s) (propertize s 'face
                                            'elfeed-search-tag-face))
                    tags ","))
         (title-width (- (window-width) 10
                         elfeed-search-trailing-width))
         (title-column (elfeed-format-column
                        title (elfeed-clamp
                               elfeed-search-title-min-width
                               title-width
                               elfeed-search-title-max-width)
                        :left))
         (authors-width 60)
         (authors-column (elfeed-format-column
                          entry-authors (elfeed-clamp
                                         elfeed-search-title-min-width
                                         authors-width
                                         131)
                          :left)))

    (insert (propertize date 'face 'elfeed-search-date-face) " ")

    (insert (propertize title-column
                        'face title-faces 'kbd-help title) " ")

    (insert (propertize authors-column
                        'face 'elfeed-search-date-face
                        'kbd-help entry-authors) " ")

    ;; (when feed-title
    ;;   (insert (propertize entry-authors
    ;; 'face 'elfeed-search-feed-face) " "))

    ;; (when entry-authors
    ;;   (insert (propertize feed-title
    ;;                       'face 'elfeed-search-feed-face) " "))

    (when tags
      (insert "(" tags-str ")"))

    )
  )

(use-package! elfeed
  :config
  (setq rmh-elfeed-org-files
        (list
         (expand-file-name "elfeed.org" ub/gtd-general-assets-dir)))
  (elfeed-org)

  ;; (defvar elfeed-feeds-alist
  ;;   '(
  ;;     ("http://export.arxiv.org/api/query?search_query=cat:cs.RO&start=0&max_results=300&sortBy=submittedDate&sortOrder=descending
  ;; " arxiv RO)
  ;;     )
  ;;   )
  (setq elfeed-search-print-entry-function #'my-search-print-fn)
  (setq elfeed-search-title-max-width 80)
  )

(run-at-time nil (* 8 60 60) #'elfeed-update) ; every 8h

(use-package! elfeed-score
  :config
  (progn
    (elfeed-score-enable)
    (define-key elfeed-search-mode-map "=" elfeed-score-map)))

(use-package! dired-subtree
  :config
  (setq dired-subtree-use-backgrounds nil)
  (let ((map dired-mode-map))
    (define-key map (kbd "<tab>") #'dired-subtree-toggle)
    (define-key map (kbd "<C-tab>") #'dired-subtree-cycle)
    (define-key map (kbd "<backtab>") #'dired-subtree-remove)))

(map! "C-x C-v" #'helm-mini)
(map! "C-x C-g" #'helm-projectile)
(map! "C-x C-'" #'+ivy/project-search-from-cwd)

(map! "C-s" #'swiper)
(map! "C-c C-r" #'ivy-resume)

(map! "C-x C-a" #'helm-org-rifle-agenda-files)

(add-hook 'prog-mode-hook 'real-auto-save-mode)
(add-hook 'org-mode-hook 'real-auto-save-mode)

;; NOTE enable global doesn't work as it is designed against  auto-save-visited-mode that is global?
;;(real-auto-save-mode)

;; (defun print-debug ()
;;   (message "debug point"))


(use-package! real-auto-save
  :config
  (setq real-auto-save-interval 5) ;; in seconds
  )

(use-package! yequake
  :custom
  (yequake-frames
   '(("org-capture"
      (buffer-fns . (yequake-org-capture))
      (width . 0.75)
      (height . 0.5)
      (alpha . 0.95)
      (frame-parameters . ((undecorated . t)
                           (skip-taskbar . t)
                           (sticky . t))))

     ("org-protocol-capture"
      (buffer-fns . (yequake-org-protocol-capture))
      (width . 0.75)
      (height . 0.5)
      (alpha . 0.95)
      (frame-parameters . ((undecorated . t)
                           (skip-taskbar . t)
                           (sticky . t))))

     ;; TODO improve eshell such as hook eshell exit and closing windows
     ;; also consider equake package since it designed for shell
     ;; and seems to has more functionality compared to yequake
     ("eshell"
      (buffer-fns . (+eshell/here))
      (width . 0.75)
      (height . 0.5)
      (alpha . 0.95)
      (frame-parameters . ((undecorated . t)
                           (skip-taskbar . t)
                           (sticky . t))))
     )))

(defun ub/org-protocol-capture (info)
  "
Instead of going passing template key, show org-capture menu"
  (let* ((parts
	  (pcase (org-protocol-parse-parameters info)
	    ;; New style links are parsed as a plist.
	    ((let `(,(pred keywordp) . ,_) info) info)
	    ;; Old style links, with or without template key, are
	    ;; parsed as a list of strings.
	    (p
	     (let ((k (if (= 1 (length (car p)))
			  '(:template :url :title :body)
			'(:url :title :body))))
	       (org-protocol-assign-parameters p k)))))
	 (template (or (plist-get parts :template)
		       org-protocol-default-template-key))
	 (url (and (plist-get parts :url)
		   (org-protocol-sanitize-uri (plist-get parts :url))))
	 (type (and url
		    (string-match "^\\([a-z]+\\):" url)
		    (match-string 1 url)))
	 (title (or (plist-get parts :title) ""))
	 (region (or (plist-get parts :body) ""))
	 (orglink
	  (if (null url) title
	    (org-link-make-string url (or (org-string-nw-p title) url))))
	 ;; Avoid call to `org-store-link'.
	 (org-capture-link-is-already-stored t))
    ;; Only store link if there's a URL to insert later on.
    (when url (push (list url title) org-stored-links))
    (org-link-store-props :type type
			  :link url
			  :description title
			  :annotation orglink
			  :initial region
			  :query parts)
    (raise-frame)
    ;; NOTE modified line
    (org-capture nil nil)
    (message "Item captured.")
    ;; Make sure we do not return a string, as `server-visit-files',
    ;; through `server-edit', would interpret it as a file name.
    nil))


;; combining with org-protocol
;; ref: https://www.reddit.com/r/emacs/comments/fjou3c/open_orgprotocol_in_a_standalone_frame/
(defun yequake-org-protocol-capture (info)
  "Call `org-protocol-capture' in a Yequake frame.
Adds a function to `org-capture-after-finalize-hook' that closes
the recently toggled Yequake frame and removes itself from the
hook.

Note: if another Yequake frame is toggled before the capture is
finalized, when the capture is finalized, the wrong Yequake frame
will be toggled."
  (let* ((remove-hook-fn (lambda ()
                           (remove-hook 'org-capture-after-finalize-hook #'yequake-retoggle))))
    (add-hook 'org-capture-after-finalize-hook remove-hook-fn)
    (add-hook 'org-capture-after-finalize-hook #'yequake-retoggle)
    ;; MAYBE: Propose an `org-capture-switch-buffer-fn' variable that could be rebound here.

    ;; NOTE: We override `org-switch-to-buffer-other-window' because
    ;; it always uses `switch-to-buffer-other-window', and we want to
    ;; display the template menu and capture buffer in the existing
    ;; window rather than splitting the frame.
    (cl-letf* (((symbol-function #'org-switch-to-buffer-other-window)
                (symbol-function #'switch-to-buffer)))
      (condition-case nil
          (progn
            ;; Hacky solution for opening the drop-down window
            (yequake--toggle-frame
             "org-protocol-capture"
             '((buffer-fns nil) (width . 0.75) (height . 0.5) (alpha . 0.95) (frame-parameters (undecorated . t) (skip-taskbar . t) (sticky . t))))
            (ub/org-protocol-capture (org-protocol-parse-parameters (s-chop-prefix "org-protocol://capture\?" info) t))
            ;; Be sure to return the "CAPTURE-" buffer, which is the current
            ;; buffer at this point.
            (current-buffer))
        ((error quit)
         ;; Capture aborted: remove the hook and hide the capture frame.
         (remove-hook 'org-capture-after-finalize-hook #'yequake-retoggle)
         (yequake-retoggle)
         )))))

(setq byte-compile-warnings '(cl-functions))
(setq byte-compile-warnings '(not cl-functions))
(setq byte-compile-warnings '(not obsolete))
