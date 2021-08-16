;; (setq doom-theme 'nil)
;; (require 'disp-table)
;; (require 'nano-theme-dark)
;; (require 'nano-help)
;; (require 'nano-modeline)
;; (require 'nano-layout)

;; NOTE still not sure if it works or not,
;; the goal is to disable unnecessary info on doom-modeline
;; disabling minor mode
(use-package! minions
  :hook (doom-modeline-mode . minions-mode))
;;(setq doom-modeline-minor-modes nil)

;; Show Org Agenda tasks with heigh spacing based on clock time with org-agenda-log-mode.
;; https://orgmode.org/worg/org-hacks.html
(defun org-agenda-log-mode-colorize-block ()
  "Set different line spacing based on clock time duration."
  (save-excursion
    (let* ((colors (cl-case (alist-get 'background-mode (frame-parameters))
                     ('light
                      (list "#F6B1C3" "#FFFF9D" "#BEEB9F" "#ADD5F7"))
                     ('dark
                      (list "#aa557f" "DarkGreen" "DarkSlateGray" "DarkSlateBlue"))))
           pos
           duration)
      (nconc colors colors)
      (goto-char (point-min))
      (while (setq pos (next-single-property-change (point) 'duration))
        (goto-char pos)
        (when (and (not (equal pos (point-at-eol)))
                   (setq duration (org-get-at-bol 'duration)))
          ;; larger duration bar height
          (let ((line-height (if (< duration 15) 1.0 (+ 0.5 (/ duration 30))))
                (ov (make-overlay (point-at-bol) (1+ (point-at-eol)))))
            (overlay-put ov 'face `(:background ,(car colors) :foreground "black"))
            (setq colors (cdr colors))
            (overlay-put ov 'line-height line-height)
            (overlay-put ov 'line-spacing (1- line-height))))))))

;;(add-hook 'org-agenda-finalize-hook #'org-agenda-log-mode-colorize-block)
;; NOTE it needs to work to be useful
