(require 'subr-x)

(defun in-emacs-dir (file)
  "Return the absolute path to FILE in `user-emacs-directory'."
  (file-truename (expand-file-name file user-emacs-directory)))

(defvar tangle-init-org-file--first-time
  (not (file-exists-p (in-emacs-dir "init.elc")))
  "t if no init.elc exists.")

(defun tangle-init-org-file (&optional no-compile)
  "Tangle, `byte-compile' and load the init file from README.org.

With a prefix-argument, or a not-nil NO-COMPILE argument if
called non-interactively, don't compile the tangled file.

Return the absolute path to the tangle file if either NO-COMPILE
is nil or `debug-on-error' is not-nil, else return nil."
  (interactive "P")
  (unless (boundp 'org-babel-tangle-file)
    (require 'ob-tangle))
  ;; For some reason, I can't make `org-babel-load-file' create the
  ;; .elc file during the first init, so for now, stick to manual
  ;; tangle -> compile -> load.
  (let ((tangled (thread-last
                   ;; car isn't always default tangled file when there
                   ;; are multiple tangled files
                   (org-babel-tangle-file (in-emacs-dir "README.org")
                                          nil
                                          "emacs-lisp")
                   (member (in-emacs-dir "README.el"))
                   car)))
    ;; When first loading the config, load the tangled file anyway to
    ;; ensure that packages are setup.
    (when tangle-init-org-file--first-time
      (load-file tangled))
    (if no-compile
        tangled
      (byte-compile-file tangled)
      (rename-file (byte-compile-dest-file tangled)
                   "init.elc"
                   :ok-if-already-exists)
      (setq tangle-init-org-file--first-time nil)
      ;; Keep the init file for debug purposes.
      (if debug-on-error
          tangled
        (delete-file tangled)
        nil))))

(defun tangle-init-org-file-after-save ()
  (add-hook 'after-save-hook #'tangle-init-org-file nil t))
;; It's a bit ugly but it does the job
(setq safe-local-variable-values '((eval tangle-init-org-file-after-save)))

(when (and (not after-init-time) ; avoid recursive tangle
           tangle-init-org-file--first-time)
  (tangle-init-org-file))
