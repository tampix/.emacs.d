(setq-local tangle-init-org-file--first-time t)

(defun tangle-init-org-file ()
  "Reload and byte-compile init file from the main org file."
  (interactive)
  (require 'ob-tangle)
  ;; For some reason, I can't make `org-babel-load-file' create the
  ;; .elc file during the first init, so for now, stick to manual
  ;; tangle -> compile -> load.
  (let* ((org-file (expand-file-name "README.org" user-emacs-directory))
         (init-file (expand-file-name "init" user-emacs-directory))
         (el-file (car (org-babel-tangle-file org-file nil "emacs-lisp"))))
    (rename-file el-file init-file t)
    ;; When first loading the config, load the tangled file anyway to
    ;; ensure that packages are setup.
    (when (bound-and-true-p tangle-init-org-file--first-time)
      (load-file init-file))
    (byte-compile-file init-file t)
    ;; Keep the init file for debug purposes.
    (if debug-on-error
        (rename-file init-file (concat init-file ".el.bak") t)
      (delete-file init-file))))

(defun tangle-init-org-file-after-save ()
  (add-hook 'after-save-hook #'tangle-init-org-file nil t))

;; A bit ugly but it does the job
(setq safe-local-variable-values '((eval tangle-init-org-file-after-save)))

(tangle-init-org-file)
