(setq gc-cons-threshold most-positive-fixnum)

(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; backport features of emacs 28.1
(when (and (<= emacs-major-version 28)
           (< emacs-minor-version 1))
  (defmacro with-environment-variables (variables &rest body)
    (declare (indent 1))
    `(let (process-environment (copy-sequence process-environment))
       ,@(mapcar (lambda (e)
                   `(setenv ,(car e) ,(cadr e)))
                 variables)
       ,@body))

  (defun string-replace (from-string to-string in-string)
    (replace-regexp-in-string
     (regexp-quote from-string) to-string in-string t t))

  (defun string-search (needle haystack &optional start-pos)
    (string-match-p (regexp-quote needle) haystack start-pos))

  (defun length= (sequence len)
    (and (>= len 0)
         (= (length sequence) len)))

  (defun length> (sequence len)
    (> (length sequence) len))

  (defun length< (sequence len)
    (< (length sequence) len)))
