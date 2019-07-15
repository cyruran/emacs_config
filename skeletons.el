(defvar license-file (expand-file-name "./license.txt"))

(defun include-license ()
  (when (file-exists-p license-file)
    (insert-file license-file)
    (insert "\n")))

;; Perl skeletons begin
(define-skeleton skeleton-perl
  "Skeleton for new perl script"
  ""
  "#!/usr/bin/perl\n"
  "\n"
  (include-license)
  "use strict;\n"
  "use warnings;\n"
  "\n")

(define-skeleton skeleton-perl-test
  "Skeleton for perl test files"
  ""
  "#!/usr/bin/perl\n"
  "\n"
  (include-license)
  "use strict;\n"
  "use warnings;\n"
  "\n"
  "use Test::More;\n"
  "\n")

(define-skeleton skeleton-perl-module
  "Skeleton for perl modules"
  ""
  "package " (file-name-base (buffer-file-name)) "\n"
  "\n"
  (include-license)
  "use strict;\n"
  "use warnings;\n"
  "\n")
;; Perl skeletons end

(define-skeleton skeleton-bash
  "Skeleton for bash scripts"
  ""
  "#!/bin/bash\n"
  "\n")

(define-skeleton skeleton-python
  "Skeleton for python scripts"
  ""
  "#!/usr/bin/env python\n"
  "\n")

(defun apply-skeletons ()
  (message "Extention %s" (file-name-extension (buffer-file-name)))
  (let ((file-ext (file-name-extension (buffer-file-name))))
    (cond 
     ((string-equal file-ext "pl") (skeleton-perl))
     ((string-equal file-ext "t") (skeleton-perl-test))
     ((string-equal file-ext "pm") (skeleton-perl-module))
     ((string-equal file-ext "sh") (skeleton-bash))
     ((string-equal file-ext "py") (skeleton-python)))))

(add-to-list 'find-file-not-found-functions 'apply-skeletons)

(provide 'skeletons)
