(use-package! conda
  :config
  (setq conda-anaconda-home ub/conda-anaconda-home)
  (setq conda-env-home-directory ub/conda-env-home-directory)
  ;; (conda-env-initialize-interactive-shells)
  ;; (conda-env-initialize-eshell)
  )
(use-package! elpy
  :config
  (elpy-enable))
