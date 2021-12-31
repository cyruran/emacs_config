(mapcar (lambda (x) (or (package-installed-p x)
			 (message "%s" x)))
	'(helm
	  s
	  projectile
	  helm-projectile
	  helm-make
	  ace-jump-mode
	  multiple-cursors
	  expand-region
	  solarized-theme
	  helm-company
	  magit))
