(use-package ido
  :config
  (setq ido-enable-flex-matching t)
  (ido-everywhere t)
  (ido-mode 1))

(use-package ag
  :ensure t
  :commands (ag ag-regexp ag-project))

(use-package twilight-bright-theme
  :ensure t
  :config (load-theme 'twilight-bright t))

(use-package helm
  :ensure t
  :bind (("M-a" . helm-M-x)
         ("C-p C-o" . helm-find-files)
         ("C-p o" . helm-recentf)
         ("C-SPC" . helm-dabbrev)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-buffers-list))
  :bind (:map helm-map
              ("M-i" . helm-previous-line)
              ("M-k" . helm-next-line)
              ("M-I" . helm-previous-page)
              ("M-K" . helm-next-page)
              ("M-h" . helm-beginning-of-buffer)
              ("M-H" . helm-end-of-buffer))
  :config (progn
            (setq helm-buffers-fuzzy-matching t)
            (helm-mode 1)))

(use-package helm-descbinds
  :ensure t
  :bind ("C-h b" . helm-descbinds))
(use-package helm-files
  :bind (:map helm-find-files-map
              ("M-i" . nil)
              ("M-k" . nil)
              ("M-I" . nil)
              ("M-K" . nil)
              ("M-h" . nil)
              ("M-H" . nil)))
(use-package helm-swoop
  :ensure t
  :bind (("M-m" . helm-swoop)
         ("M-M" . helm-swoop-back-to-last-point))
  :init
  (bind-key "M-m" 'helm-swoop-from-isearch isearch-mode-map))
(use-package helm-ag
  :ensure helm-ag
  :bind ("M-p" . helm-projectile-ag)
  :commands (helm-ag helm-projectile-ag)
  :init (setq helm-ag-insert-at-point 'symbol
              helm-ag-command-option "--path-to-agignore ~/.agignore"))

(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  (setq projectile-enable-caching t))

(use-package helm-projectile
  :ensure t
  :bind (("C-x o" . helm-projectile-find-file)
         ("M-p" . helm-projectile-find-file)
         ("C-x r" . helm-projectile-grep))
  :config (helm-projectile-on))

(use-package ruby-mode
  :ensure t
  :defer t
  :mode (("\\.rb\\'"       . ruby-mode)
         ("\\.ru\\'"       . ruby-mode)
         ("\\.jbuilder\\'" . ruby-mode)
         ("\\.gemspec\\'"  . ruby-mode)
         ("\\.rake\\'"     . ruby-mode)
         ("Rakefile\\'"    . ruby-mode)
         ("Gemfile\\'"     . ruby-mode)
         ("Guardfile\\'"   . ruby-mode)
         ("Capfile\\'"     . ruby-mode)
         ("Vagrantfile\\'" . ruby-mode))
  :config (progn
            (setq ruby-indent-level 2
                  ruby-deep-indent-paren nil
                  ruby-bounce-deep-indent t
                  ruby-hanging-indent-level 2))
  :init (add-to-list 'auto-mode-alist '("Brewfile" . ruby-mode)))

(use-package rubocop
  :ensure t
  :defer t
  :init (add-hook 'ruby-mode-hook 'rubocop-mode))


(use-package minitest
  :bind ("M-e" . minitest-verify)
  :config (progn
            (defun minitest-ruby-mode-hook ()
              (tester-init-test-run #'minitest-run-file "_test.rb$")
              (tester-init-test-suite-run #'minitest-verify-all))

            (add-hook 'ruby-mode-hook 'minitest-mode)))

(use-package rspec-mode
  :ensure t
  :defer t
  :config (progn
            (defun rspec-ruby-mode-hook ()
              (tester-init-test-run #'rspec-run-single-file "_spec.rb$")
              (tester-init-test-suite-run #'rake-test))
            (add-hook 'ruby-mode-hook 'rspec-ruby-mode-hook)))

(use-package rbenv
  :ensure t
  :defer t
  :init (setq rbenv-show-active-ruby-in-modeline nil)
  :config (progn
            (global-rbenv-mode)
            (add-hook 'ruby-mode-hook 'rbenv-use-corresponding)))

(use-package flycheck
  :ensure t
  :defer 5
  :config (progn
            (global-flycheck-mode 1)
            ;; https://github.com/purcell/exec-path-from-shell
            ;; only need exec-path-from-shell on OSX
            ;; this hopefully sets up path and other vars better
            (exec-path-from-shell-initialize)

            ;; disable jshint since we prefer eslint checking
            (setq-default flycheck-disabled-checkers
                          (append flycheck-disabled-checkers
                                  '(javascript-jshint)))

            ;; use eslint with web-mode for jsx files
            (flycheck-add-mode 'javascript-eslint 'web-mode)
            (flycheck-add-mode 'javascript-eslint 'js2-mode)

            ;; customize flycheck temp file prefix
            (setq-default flycheck-temp-prefix ".flycheck")

            ))

(use-package drag-stuff
  :ensure t
  :bind (("C-M-i" . drag-stuff-up)
         ("C-M-k" . drag-stuff-down)
         ("C-M-l" . python-indent-shift-right)
         ("C-M-j" . python-indent-shift-left)))

(use-package magit
  :ensure t
  :defer 2
  :bind (("C-x g" . magit-status)))

(use-package slim-mode
  :ensure t
  :mode ("\\.slim\\'" . slim-mode))

(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'" . yaml-mode))


(defun django-web-mode ()
  "Enable django web mode."
  (interactive)
  (web-mode-set-engine "django"))

(use-package web-mode
  :ensure t
  :mode (("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.php\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.djjson\\'" . web-mode)
         ("\\.scss\\'" . web-mode)
         ("\\.vue\\'" . web-mode))
  :config (progn
            (setq web-mode-markup-indent-offset 2
                  web-mode-css-indent-offset 2
                  web-mode-code-indent-offset 2
                  web-mode-script-padding 2
                  web-mode-style-padding 2))
  :init (bind-key "M-w d" 'django-web-mode))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode))

(use-package coffee-mode
  :ensure t
  :mode ("\\.coffee\\'" . coffee-mode)
  :config (progn
            (setq coffee-indent-tabs-mode nil
                  coffee-tab-width 2)))

(use-package js2-mode
  :ensure t
  :mode (("\\.es6\\'" . js2-mode)
         ("\\.js\\'" . js2-mode))
  :config (progn
            (setq js2-basic-offset 2)))

(use-package robe
  :ensure t
  :bind (("C-r C-j" . robe-jump)
         ("C-r C-r" . robe-rails-refresh)
         ("C-r C-s" . robe-start)))
(add-hook 'ruby-mode-hook 'robe-mode)

(use-package perspective
  :ensure t
  :bind (("C-p s" . persp-switch)
         ("C-p p" . jone-persp-switch-last)
         ("C-p k" . persp-kill))
  :config (persp-mode))

(use-package persp-projectile
  :ensure t
  :bind (("C-x p" . projectile-persp-switch-project)))


(use-package jone
  :bind (("C-c f c" . jone-make-changelog-entry)))


(provide 'packages-cfg)
