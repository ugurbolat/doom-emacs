(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline success warning error])
 '(ansi-color-names-vector
   ["gray35" "#a60000" "#005e00" "#813e00" "#0031a9" "#721045" "#00538b" "gray65"])
 '(awesome-tray-mode-line-active-color "#0031a9")
 '(awesome-tray-mode-line-inactive-color "#d7d7d7")
 '(custom-safe-themes
   '("d81c895eadb0e60e6608aca22200684d54f29d4b7e8a4550a83139de6ddee7db" "f65451261f23f770b06f2ea96a9d349c5c7eaec15cc057873010f2f11a4c7229" "e9d47d6d41e42a8313c81995a60b2af6588e9f01a1cf19ca42669a7ffd5c2fde" "076ee9f2c64746aac7994b697eb7dbde23ac22988d41ef31b714fc6478fee224" "2b502f6e3bf0cba42fe7bf83a000f2d358a7020a7780a8682fcfed0c9dbffb5f" "63bfcabeb44559c67d8827dc68cd6c4a6d3ce35ef4504343af12d42f24894e00" default))
 '(exwm-floating-border-color "#888888")
 '(fci-rule-color "#5B6268")
 '(flymake-error-bitmap '(flymake-double-exclamation-mark modus-themes-fringe-red))
 '(flymake-note-bitmap '(exclamation-mark modus-themes-fringe-cyan))
 '(flymake-warning-bitmap '(exclamation-mark modus-themes-fringe-yellow))
 '(highlight-tail-colors '(("#aecf90" . 0) ("#c0efff" . 20)))
 '(hl-todo-keyword-faces
   '(("HOLD" . "#70480f")
     ("TODO" . "#721045")
     ("NEXT" . "#5317ac")
     ("THEM" . "#8f0075")
     ("PROG" . "#00538b")
     ("OKAY" . "#30517f")
     ("DONT" . "#315b00")
     ("FAIL" . "#a60000")
     ("BUG" . "#a60000")
     ("DONE" . "#005e00")
     ("NOTE" . "#863927")
     ("KLUDGE" . "#813e00")
     ("HACK" . "#813e00")
     ("TEMP" . "#5f0000")
     ("FIXME" . "#a0132f")
     ("XXX+" . "#972500")
     ("REVIEW" . "#005a5f")
     ("DEPRECATED" . "#201f55")))
 '(ibuffer-deletion-face 'modus-themes-mark-del)
 '(ibuffer-filter-group-name-face 'modus-themes-pseudo-header)
 '(ibuffer-marked-face 'modus-themes-mark-sel)
 '(ibuffer-title-face 'default)
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(objed-cursor-color "#ff6c6b")
 '(org-src-block-faces 'nil)
 '(pdf-view-midnight-colors '("#000000" . "#f8f8f8"))
 '(rustic-ansi-faces
   ["#282c34" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#bbc2cf"])
 '(safe-local-variable-values
   '((eval setq org-roam-db-location
           (expand-file-name ".vdb.db" ub/vdb))
     (eval setq org-roam-directory ub/vdb)
     (eval setq org-roam-db-location
           (expand-file-name ".gtd-roam.db" ub/gtd-root-dir))
     (eval setq org-roam-db-location
           (expand-file-name ".phd-roam.db" ub/gtd-root-dir))
     (eval setq org-roam-directory ub/gtd-root-dir)
     (eval setq org-roam-db-location
           (expand-file-name ".phd-roam.db" ub/zk-phd-dir))
     (eval setq org-roam-directory ub/zk-phd-dir)
     (eval setq org-roam-db-location
           (expand-file-name ".refile-roam.db" ub/zk-refile-dir))
     (eval setq org-roam-directory ub/zk-refile-dir)
     (eval setq org-roam-db-location
           (expand-file-name ".shared-roam.db" ub/zk-shared-dir))
     (eval setq org-roam-db-location
           (expand-file-name ".private-roam.db" ub/zk-private-dir))
     (eval org-mode-restart)
     (eval setq org-superstar-mode -1)
     (eval setq org-roam-db-location
           (expand-file-name "private-roam.db" ub/zk-private-dir))
     (eval setq org-roam-directory ub/zk-private-dir)
     (eval setq org-roam-db-location
           (expand-file-name "shared-roam.db" ub/zk-shared-dir))
     (eval setq org-roam-directory ub/zk-shared-dir)
     (eval setq org-download-heading-lvl nil)
     (eval setq org-download-image-dir "~/main/org/gtd/attachments")
     (eval setq org-download-method 'directory)
     (eval setq org-roam-db-location
           (expand-file-name ".gtd.db" "~/main/org/gtd/"))
     (eval setq org-roam-directory "~/main/org/gtd/")))
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#a60000")
     (40 . "#721045")
     (60 . "#8f0075")
     (80 . "#972500")
     (100 . "#813e00")
     (120 . "#70480f")
     (140 . "#5d3026")
     (160 . "#184034")
     (180 . "#005e00")
     (200 . "#315b00")
     (220 . "#005a5f")
     (240 . "#30517f")
     (260 . "#00538b")
     (280 . "#093060")
     (300 . "#0031a9")
     (320 . "#2544bb")
     (340 . "#0000c0")
     (360 . "#5317ac")))
 '(vc-annotate-very-old-color nil)
 '(xterm-color-names
   ["black" "#a60000" "#005e00" "#813e00" "#0031a9" "#721045" "#00538b" "gray65"])
 '(xterm-color-names-bright
   ["gray35" "#972500" "#315b00" "#70480f" "#2544bb" "#8f0075" "#30517f" "white"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-current-time ((t (:inherit org-time-grid :foreground "#eb6c63"))))
 '(org-headline-done ((t (:inherit variable-pitch :strike-through t)))))
