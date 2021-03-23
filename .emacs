
;; Jim Van Donsel's .emacs file
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))


;; The one package to rule them all.
;; Auto-downloads things wrapped in 'use-package'.
(require 'use-package)

; Follow buffers
(setq mouse-autoselect-window t)

;; Get rid of c-z minimizing the window
(global-set-key "\C-z" nil)

;; Text resizing
(global-set-key (kbd "C-_") 'text-scale-decrease)
(global-set-key (kbd "C-+") 'text-scale-increase)

(ctags-global-auto-update-mode)
(setq ctags-update-prompt-create-tags nil);you need manually create TAGS in your project

; Mac
(setenv "PATH" (concat (getenv "PATH") ":/Users/jdonsel/bin"))
(setq exec-path (append exec-path '("/Users/jdonsel/bin")))

(use-package misc)




(use-package markdown-mode)
(use-package tabbar)
(use-package web-mode)

(setq inhibit-splash-screen t)
(setq visible-bell nil)
;(setq tags-file-name "~/tags")

; truncate lines
(setq-default truncate-lines t)
; enable column numbering
(column-number-mode t)
; Yank will overwrite current selection
(delete-selection-mode 1)

;(setq elpy-rpc-virtualenv-path ’current)

; no backups
; line numbers
;(global-linum-mode 1)
(setq make-backup-files nil)

; Highlight current line
(global-hl-line-mode t)
(set-face-background 'hl-line "#E4E4E4")

(setq grep-command "grep -r -nH ")
; Sentences end in a period and a single space
(setq sentence-end-double-space nil)
; Count lines even if they are long
(setq line-number-display-limit-width 10000)
;; yes-or-no -> y-or-n
(fset 'yes-or-no-p 'y-or-n-p)
; Treat '_' as a word character
(setq c-mode-hook '(lambda ()   (modify-syntax-entry ?_ "w")))
; Force horizontal splitting                                                                                                                 
(setq split-width-threshold nil)
(global-set-key [f3] 'buffer-menu)
(global-set-key [f4] 'find-file)
; revert buffer
(global-set-key "\M-r" 'revert-buffer)
(global-set-key [(control x) (control b)] 'buffer-menu)
(global-set-key "\M-g" 'goto-line)
(global-set-key (kbd "C-o") 'open-next-line)
(global-set-key "\M-f" 'forward-to-word)
;(global-set-key [\C-right] 'forward-to-word)
(global-set-key "\C-xg" 'rgrep)
(global-set-key [\C-up] '(lambda () (interactive) (previous-line) (previous-line) (previous-line) (previous-line) (previous-line)))
(global-set-key [\C-down] '(lambda () (interactive) (next-line) (next-line) (next-line) (next-line) (next-line)))
(global-set-key "\M-t" 'toggle-truncate-lines)
(global-set-key "\C-xl" 'copy-line)
(global-set-key "\C-x\C-o" 'other-window)
(global-set-key "\C-xm" 'my-bookmark-set)
(global-set-key "\C-xj" 'my-bookmark-jump)
; Prompts for a background color for selection
(global-set-key "\C-\M-y" 'facemenu-set-background)

global-map

;; org mode
(use-package org 
  :config
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (define-key org-mode-map (kbd "M-a") 'outline-show-all)
  (add-hook 'org-mode-hook 'visual-line-mode) 

  ;; Source-block
  (defun mark-region-as-src () (interactive)
         (save-excursion
           (goto-char (region-end))
           (insert "#+END_SRC\n")
           (goto-char (region-beginning))
           (insert "#+BEGIN_SRC\n")
           )
         )
  (define-key org-mode-map (kbd "M-s") 'mark-region-as-src)

  )
(use-package org-tree-slide
  :config
  (setq org-hide-emphasis-markers t)
  (setq org-hide-leading-stars t)
  (add-hook 'org-mode-hook (lambda () (set-face-attribute 'org-level-1 nil :height 2.0)))
  (add-hook 'org-mode-hook (lambda () (set-face-attribute 'org-level-2 nil :height 1.3)))
  (add-hook 'org-mode-hook (lambda () (set-face-attribute 'org-level-2 nil :height 0.8)))
  )

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  ;; Eliminate all header bullets 
  (setq org-bullets-bullet-list '("\u200b"))
  )


;; Helm
(use-package helm)
(use-package helm-ag
  :config
  ;;(global-set-key "\C-x\C-f" 'helm-find-files)
  (global-set-key "\C-\M-g" 'helm-do-ag)
  (global-set-key "\M-i" 'helm-projectile-ag)
  )
(use-package projectile)
(use-package helm-projectile
  :config
  (projectile-global-mode 1)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)
  (define-key projectile-mode-map (kbd "C-c p") 'helm-projectile)
  )


;; Clojure
; this gets invoked by 'run-lisp'
(setq inferior-lisp-program "~/bin/clojure")
(add-hook 'clojure-mode-hook 'prettify-symbols-mode)

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
)

;; Kill whitespace
(global-set-key "\C-\M-w" 'fixup-whitespace)


;; Paredit
(use-package paredit
  :config
  (add-hook 'clojure-mode-hook 'paredit-mode)
  ;; Redefine some paredit keys
  ;; First we need to undefine the keys in paredit's map
  (eval-after-load 'paredit '(progn (define-key paredit-mode-map [\C-\M-right] nil)))
  (eval-after-load 'paredit '(progn (define-key paredit-mode-map [\C-\M-left] nil)))
  (global-set-key [\C-\M-right] 'paredit-forward)
  (global-set-key [\C-\M-left] 'paredit-backward)
  )
 
; Recursive edit
;; Enter a recursive edit. C-M-c will bring back exactly there
(global-set-key (kbd "C-c r") (lambda ()
                                (interactive)
                                (message "Hit C-M-c to exit recursive edit")
                                (save-window-excursion
                                  (save-excursion
                                    (recursive-edit)))))
(defun my-bookmark-jump (r)
  (interactive "cRegister Name:")
  (jump-to-register r)
)
(defun my-bookmark-set (r)
  (interactive "cRegister Name:")
  (point-to-register r)
)
;; ^E in Vi
(defun ctrl-e-in-vi (n)
 (interactive "p")
 (scroll-down n))
;; ^Y in Vi
(defun ctrl-y-in-vi (n)
 (interactive "p")
 (scroll-up n))
(global-set-key "\M-n" 'ctrl-y-in-vi)
(global-set-key "\M-p" 'ctrl-e-in-vi)
 
; undo is already bound to C-/
(menu-bar-enable-clipboard)
(put 'dired-find-alternate-file 'disabled nil)
(add-to-list 'auto-mode-alist '("\\.clj" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.kmap" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.jsp" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tag" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.ts" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.cpp" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h" . c++--mode))
(add-to-list 'auto-mode-alist '("\\.html" . web-mode))

(define-key minibuffer-local-map [f3]
  (lambda() (interactive) (insert (buffer-file-name (nth 1 (buffer-list))))))
(put 'upcase-region 'disabled nil)
 (setq c-default-style "linux"
          c-basic-offset 4)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(auto-compression-mode t nil (jka-compr))
 '(auto-hscroll-mode t)
 '(case-fold-search t)
 '(column-number-mode t)
 '(current-language-environment "UTF-8")
 '(custom-enabled-themes '(whiteboard))
 '(default-input-method "rfc1345")
 '(focus-follows-mouse t)
 '(font-use-system-font nil)
 '(global-font-lock-mode t nil (font-lock))
 '(horizontal-scroll-bar-mode t)
 '(indent-tabs-mode nil)
 '(ispell-highlight-face 'flyspell-incorrect)
 '(ispell-local-dictionary "american")
 '(ispell-program-name "/opt/local/bin/aspell")
 '(large-file-warning-threshold 1000000000)
 '(org-indent-indentation-per-level 2)
 '(org-list-indent-offset 8)
 '(org-log-done t)
 '(org-startup-indented t)
 '(package-archives
   '(("elpa" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.org/packages/")))
 '(package-selected-packages
   '(use-package org-bullets org-tree-slide org-translate projectile helm-projectile markdown-mode helm-ag-r helm-org helm-ag helm ag xref-js2 ggtags evil evil-visual-mark-mode ctags-update auto-virtualenv virtualenv elpy web-mode ess rainbow-mode tabbar rainbow-delimiters paredit magit json-mode js2-mode flymake-jslint company ac-cider 0blayout))
 '(recentf-menu-filter 'recentf-sort-ascending)
 '(recentf-mode t nil (recentf))
 '(ring-bell-function 'ignore)
 '(save-place t nil (saveplace))
 '(save-place-mode t nil (saveplace))
 '(scroll-bar-mode 'right)
 '(scroll-conservatively 10)
 '(scroll-down-aggressively nil)
 '(scroll-step 1)
 '(scroll-up-aggressively nil)
 '(show-paren-mode t nil (paren))
 '(speedbar-show-unknown-files t)
 '(speedbar-tag-hierarchy-method '(speedbar-simple-group-tag-hierarchy))
 '(tabbar-mode t nil (tabbar))
 '(tabbar-mwheel-mode t nil (tabbar))
 '(tool-bar-mode nil nil (tool-bar))
 '(uniquify-buffer-name-style 'forward nil (uniquify)))
(setq truncate-partial-width-windows 1)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "whitesmoke" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "1ASC" :family "Liberation Mono"))))
 '(font-lock-comment-face ((((class color) (background light)) (:foreground "Dark Green"))))
 '(font-lock-string-face ((((class color) (background light)) (:foreground "Red"))))
 '(mode-line ((t (:background "MediumPurple1" :foreground "White" :box (:line-width -1 :style released-button)))))
 '(tabbar-button ((t (:inherit default :box (:line-width 1 :color "white" :style released-button)))))
 '(tabbar-default ((t (:inherit variable-pitch :background "gray75" :foreground "gray50" :height 1.0)))))

;; Frame title bar formatting to show full path of file
(setq-default
 frame-title-format
 (list '((buffer-file-name " %f" (dired-directory
                  dired-directory
                  (revert-buffer-function " %b"
                  ("%b - Dir:  " default-directory)))))))
;; Behave like vi's o command
(defun open-next-line (arg)
  "Move to the next line and then opens a line.
    See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1))
 
(put 'narrow-to-region 'disabled nil)
(defun copy-line ()
  (interactive)
  (beginning-of-line )
  (let ((beg (point))) (forward-line 1) (copy-region-as-kill beg (point))) (forward-line -1)
  (message "copied line")
)
 
   
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
 
;; Flymake - lint for python
(when (load "flymake" t)
      (defun flymake-pylint-init ()
        (let* ((temp-file (flymake-init-create-temp-buffer-copy
                           'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
          (list "/usr/local/bin/epylint" (list local-file))))
 (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))

;; ====================
;; insert date and time
(defvar current-date-time-format "%a %b %d %H:%M:%S %Z %Y"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defun insert-current-date-time ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
       (interactive)
       (insert (format-time-string current-date-time-format (current-time)))
       )

(global-set-key "\C-\M-d" 'insert-current-date-time)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time

(setq mouse-wheel-progressive-speed 0.05) ;; scroll acceleration

(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(setq scroll-step 1) ;; keyboard scroll one line at a time

(use-package xref-js2)
(use-package js
  :config
  (add-hook 'js2-mode-hook (lambda ()  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
  (add-hook 'js2-mode-hook (lambda () (define-key js2-mode-map (kbd "M-.") nil)))
)

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-read-string-input             'from-child-frame
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35
          treemacs-workspace-switch-cleanup      nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("<f8>"      . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
  )

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

;; Magit
(use-package magit
  :config
  (define-key global-map (kbd "M-m") 'magit)
  )


;; Evil mode
(defun evil ()  
  (interactive)
  (evil-mode 1)
  )

;; Holy mode
(defun holy ()  
  (interactive)
  (evil-mode 0)
  )

(use-package evil)
(use-package evil-states
  :config
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "C-s") 'save-buffer)
  (define-key evil-normal-state-map (kbd "C-S-S") 'isearch-forward)
  (define-key evil-insert-state-map (kbd "C-s") '(lambda () (interactive) (evil-force-normal-state) (save-buffer)))
  )

;; Use evil mode by default
(evil)

;; Use helm
(helm-mode)

;; Frame size
(when window-system (set-frame-size (selected-frame) 150 50))
