;; -*- mode: emacs-lisp -*-

;; Emacs config
;; Peter Sujan
;; Last updated: 3/13/15

;;; timing
(message "Begin loading Peter's .emacs file")
(require 'cl) ; a rare necessary use of REQUIRE
(defvar *emacs-load-start* (current-time))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE INSTALLATION ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (>= emacs-major-version 24)
 (require 'package)
 (add-to-list
  'package-archives
  '("melpa-stable" . "http://stable.melpa.org/packages/")
  t)
 (add-to-list
  'package-archives
  '("marmalade" . "https://marmalade-repo.org/packages/")
  t)
 (package-initialize)
 (package-refresh-contents)
  )

(add-to-list 'load-path "~/.emacs.d/lisp")

; (setq x-select-enable-clipboard t)


;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL SETTINGS ;;
;;;;;;;;;;;;;;;;;;;;;;

; suppresses splash screen
(setq inhibit-splash-screen t)

; starts emacs maximized
; instead accomplished by aliasing emacs to emacs -mm
;(custom-set-variables
; '(initial-frame-alist (quote ((fullscreen . maximized)))))

; Makes M-<direction> switch windows
(global-set-key (kbd "<M-up>") 'windmove-up)
(global-set-key (kbd "<M-down>") 'windmove-down)
(global-set-key (kbd "<M-left>") 'windmove-left)
(global-set-key (kbd "<M-right>") 'windmove-right)

; Highlight matching parens
(show-paren-mode 1)
(setq show-paren-delay 0)

; 'FIXME in comments' mode -- highlights TODOs and FIXMEs
(require 'fic-mode)
(add-hook 'c-mode-common-hook 'turn-on-fic-mode)
(add-hook 'js-mode-hook 'turn-on-fic-mode)

;;;;;;;;;;;;;;;;;;;
;; MODE SPECIFIC ;;
;;;;;;;;;;;;;;;;;;;

;;;; HTML MODE
; Sets the indentation level for html mode
(add-hook 'html-mode-hook
        (lambda ()
          ;; Default indentation is usually 2 spaces, changing to 4.
          (set (make-local-variable 'sgml-basic-offset) 4)
	  (set 'fill-column 120)))


;;;; OCTAVE MODE
; Sets .m files to octave mode
(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))

;;;; JAVASCRIPT/JSON
; Adds json formatting command
(defun json-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)
    )
  )

; make .json files open in js-mode
(setq auto-mode-alist
      (cons '("\\.json$" . js-mode) auto-mode-alist))

; Decrease js/json indentation
(add-hook 'js-mode-hook
	  (lambda ()
	    (set (make-local-variable 'js-indent-level) 4)
	    )
	  )

;;;; YAML
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;;;; WEB MODE
(require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
;; my customizations
;; (setq web-mode-code-indent-offset 2)

;;;; AUTOCOMPLETE
(add-to-list 'load-path "~/.emacs.d/auto-complete")
(when (require 'auto-complete-config nil 'noerror)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/ac-dict")
  (ac-config-default))


;;;; ESS
;;; ESS related stuff - suggested by http://kieranhealy.org/blog/archives/2009/10/12/make-shift-enter-do-a-lot-in-ess/
;;; There seems to be some bug in which it complains about an unexpected
;;; '>' when R first starts
(add-to-list 'load-path "~/.emacs.d/lisp/ESS/lisp/")
(require 'ess-site)
(setq ess-ask-for-ess-directory nil)
(setq ess-local-process-name "R")
(setq ansi-color-for-comint-mode 'filter)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)
(defun my-ess-start-R ()
  (interactive)
  (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
      (progn
        (delete-other-windows)
        (setq w1 (selected-window))
        (setq w1name (buffer-name))
        (setq w2 (split-window w1 nil t))
        (R)
        (set-window-buffer w2 "*R*")
        (set-window-buffer w1 w1name))))
(defun my-ess-eval ()
  (interactive)
  (my-ess-start-R)
  (if (and transient-mark-mode mark-active)
      (call-interactively 'ess-eval-region)
    (call-interactively 'ess-eval-line-and-step)))

(add-hook 'ess-mode-hook
          '(lambda()
             (local-set-key [C-return] 'my-ess-eval)))

;; Simpler alternative to the above
;; (add-hook 'ess-mode-hook
;; 	  (lambda()
;; 	    (local-set-key [C-return] 'ess-eval-line-and-step)))

;;;; LATEX
;; Didn't really like this
;;(add-hook 'LaTeX-mode-hook
;;	  (lambda()
;;	    (local-set-key [tab] 'TeX-complete-symbol)))

;;;; MARKDOWN
(require 'markdown-mode)

;;;; POLYMODE
(add-to-list 'load-path "~/.emacs.d/lisp/polymode/")
(add-to-list 'load-path "~/.emacs.d/lisp/polymode/modes")
(require 'poly-R)
(require 'poly-markdown)
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
(defalias 'bash-mode 'shell-script-mode)


;;;;;;;;;;;;;;
;; ASSORTED ;;
;;;;;;;;;;;;;;

; Removes weird characters caused by copying from google docs
(defun remove-google-docs-garbage ()
  (interactive)
  (format-replace-strings 
   '(
     ("“" . "\"")
     ("”" . "\"")
     ("‘" . "\'")
     ("’" . "\'"))
   )
)

; binds the above procedure to the F6 key
(global-set-key [f6] 'remove-google-docs-garbage)


;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))


;;;;;;;;;;;;;;;;;;;;
;; FROM CUSTOMIZE ;;
;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "pdflatex")
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(c-basic-offset 4)
 '(custom-enabled-themes (quote (light-blue)))
 '(custom-safe-themes (quote ("01b2830f44925d13b3e34eba4d1dd34af4c6c197aeb53fbe0f52aefe13e60f0d" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(org-agenda-files (quote ("~/school/todo.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;;;;;;;;;;;;;;;;;
;; COLOR THEMES ;;
;;;;;;;;;;;;;;;;;;

;;; color themes - not done yet
;(require 'color-theme-solarized)
;(color-theme-solarized)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes" t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized" t)

;;; Timing -- this fails in emacs 24
(when (< emacs-major-version 24)
  (message "My .emacs loaded in %ds" (destructuring-bind (hi lo ms) (current-time)
				       (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))
  )

