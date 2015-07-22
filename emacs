; -*- mode: emacs-lisp -*-

;; Emacs config
;; Peter Sujan
;; Last updated: 3/13/15

;;; timing
(message "Begin loading Peter's .emacs file")
(require 'cl) ; a rare necessary use of REQUIRE
(defvar *emacs-load-start* (current-time))

; (setq x-select-enable-clipboard t)

; Sets the indentation level for html mode
(add-hook 'html-mode-hook
        (lambda ()
          ;; Default indentation is usually 2 spaces, changing to 4.
          (set (make-local-variable 'sgml-basic-offset) 4)
	  (set 'fill-column 120)))

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

(defun json-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)
)
)

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(LaTeX-command "pdflatex")
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (light-blue))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;; Adds autocomplete

(add-to-list 'load-path "~/.emacs.d/auto-complete")
(when (require 'auto-complete-config nil 'noerror)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/ac-dict")
  (ac-config-default))

;;; ESS related stuff - suggested by http://kieranhealy.org/blog/archives/2009/10/12/make-shift-enter-do-a-lot-in-ess/
;;; There seems to be some bug in which it complains about an unexpected
;;; '>' when R first starts
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

;; Didn't really like this
;;(add-hook 'LaTeX-mode-hook
;;	  (lambda()
;;	    (local-set-key [tab] 'TeX-complete-symbol)))
;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))




;;; Timing
(message "My .emacs loaded in %ds" (destructuring-bind (hi lo ms) (current-time)
						       (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))