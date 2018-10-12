(defun tangle-init-org-file ()
  "Reload and byte-compile init file from the main org file."
  (interactive)
  (let ((default-directory user-emacs-directory))
    (require 'org)
    ;; tangle `README.org' and byte-compile the generated .el file
    (org-babel-tangle-file "README.org")
    (byte-compile-file "README.el" t)
    ;; set byte-compiled file as init file
    (rename-file "README.elc" "init.elc" t)
    ;; keep a backup of generated init file for debugging purposes
    (rename-file "README.el" ".init.el.bak" t)))

(tangle-init-org-file)
