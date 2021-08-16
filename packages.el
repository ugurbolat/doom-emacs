;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
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
;; our package manager can't deal with; see raxod502/straight.el#279)
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

(package! aggressive-indent)
(package! org-gcal
  :recipe (:host github :repo "kidd/org-gcal.el")
  :pin "133cca813abd2823a6e2a9ada295b7b8b115be4f")
(package! org-download)
(package! org-cliplink)
(package! org-super-agenda
    ;;:recipe (:host github :repo "alphapapa/org-super-agenda"))
    :recipe (:host github :repo "ugurbolat/org-super-agenda"))
(package! doct)

;; pinned org-roam packages as of 11.08.21
(package! org-roam
  :recipe (:host github :repo "org-roam/org-roam")
  :pin "5dce6261a2a45798cdf0c65371b76c83dd5b1de6")
;; for org-roam v2, instead of org-roam-server
(package! websocket)
(package! org-roam-ui
  :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out"))
  :pin "b153f4fee99e36dec0fb56d987026d53bf97a0e8")
(package! org-roam-bibtex
  :recipe (:host github :repo "org-roam/org-roam-bibtex")
  :pin "c13a05b2c855ba1516241d8a1de33bf2c689d6e4")

;; When using org-roam via the `+roam` flag
(unpin! org-roam company-org-roam)
;; When using bibtex-completion via the `biblio` module
(unpin! bibtex-completion helm-bibtex ivy-bibtex)

(package! helm-bibtex
  :recipe (:host github :repo "tmalsburg/helm-bibtex"))
(package! org-ref
  :recipe (:host github :repo "jkitchin/org-ref"))
;; NOTE org-ref opens pdf with system pdf viewer tool
;; so this package allows it to view with pdftools
(package! org-pdftools
  :recipe (:host github :repo "fuxialexander/org-pdftools"))
(package! projectile
  :recipe (:host github :repo "bbatsov/projectile"))
(package! org-noter
  :recipe (:host github :repo "weirdNox/org-noter"))
(package! org-now
  :recipe (:host github :repo "alphapapa/org-now"))
(package! org-pomodoro
  :recipe (:host github :repo "marcinkoziej/org-pomodoro"))
(package! org-journal
  :recipe (:host github :repo "bastibe/org-journal"))
(package! org-pandoc-import
  :recipe (:host github
           :repo "tecosaur/org-pandoc-import"
           :files ("*.el" "filters" "preprocessors")))
(package! unfill
    :recipe (:host github :repo "purcell/unfill"))
(package! activity-watch-mode
    :recipe (:host github :repo "pauldub/activity-watch-mode"))
(package! elgantt
  :recipe (:host github :repo "legalnonsense/elgantt"))
(package! org-ql
  :recipe (:host github :repo "alphapapa/org-ql"))
(package! ts
  :recipe (:host github :repo "alphapapa/ts.el"))
(package! ox-gfm
  :recipe (:host github :repo "larstvei/ox-gfm"))
(package! minions
  :recipe (:host github :repo "tarsius/minions"))
(package! all-the-icons-dired
  :recipe (:host github :repo "jtbm37/all-the-icons-dired"))
(package! modus-themes
  :recipe (:host github :repo "protesilaos/modus-themes"))
(package! org-super-links
  :recipe (:host github :repo "toshism/org-super-links"))
(package! w32-browser)
(package! org-mind-map
  :recipe (:host github :repo "the-ted/org-mind-map"))
(package! emacs-conflict
  :recipe (:host github :repo "ibizaman/emacs-conflict"))
(package! alert
  :recipe (:host github :repo "ugurbolat/alert"))
(package! elfeed
  :recipe (:host github :repo "skeeto/elfeed"))
(package! elfeed-org
  :recipe (:host github :repo "remyhonig/elfeed-org"))
(package! elfeed-score
  :recipe (:host github :repo "sp1ff/elfeed-score"))
(package! nov)
(package! org-media-note
  :recipe (:host github :repo "yuchen-lea/org-media-note"))
(package! pdf-continuous-scroll-mode
  :recipe (:host github :repo "dalanicolai/pdf-continuous-scroll-mode.el"))
(package! dired-hacks
  :recipe (:host github :repo "Fuco1/dired-hacks"))
(package! org-mru-clock
  :recipe (:host github :repo "unhammer/org-mru-clock"))
(package! centaur-tabs
  :recipe (:host github :repo "ema2159/centaur-tabs"))
(package! org-sidebar
  :recipe (:host github :repo "alphapapa/org-sidebar"))
(package! helm-org-rifle
  :recipe (:host github :repo "alphapapa/helm-org-rifle"))
(package! org-toggl
  :recipe (:host github :repo "Fuco1/org-toggl"))
(package! evil-tutor)
(package! real-auto-save
  :recipe (:host github :repo "ChillarAnand/real-auto-save"))
(package! org-clock-budget
  :recipe (:host github :repo "Fuco1/org-clock-budget"))
(package! elpy
  :recipe (:host github :repo "jorgenschaefer/elpy"))
;; drop-down emacs frames
(package! yequake
  :recipe (:host github :repo "alphapapa/yequake"))
(package! s.el
  :recipe (:host github :repo "magnars/s.el"))
(package! csv-mode)
