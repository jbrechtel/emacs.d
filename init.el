(global-font-lock-mode t)

(windmove-default-keybindings)

(setq use-file-dialog nil)

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Highlight regions and add special behaviors to regions.
;; "C-h d transient" for more info
(setq transient-mark-mode t)

;; Display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

(global-set-key "\C-c\C-w" 'backward-kill-word)

;; Remove unnecessary gui stuff
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode 1))

(blink-cursor-mode 0) ;; no blink

;;semantic-ectag-util.el

(defcustom semantic-ectag-program "/usr/bin/ctags-exuberant"
  "The Exuberent CTags program to use."
  :group 'semantic
  :type 'program)

(require 'package)
(add-to-list 'package-archives 
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))

(package-initialize)
(ido-mode)
(evil-mode 1)

;; fix the PATH variable when launching from something like OSXAabcK
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(defun my-move-key (keymap-from keymap-to key)
       "Moves key binding from one keymap to another, deleting from the old location. "
            (define-key keymap-to key (lookup-key keymap-from key))
	         (define-key keymap-from key nil))
   (my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
   (my-move-key evil-motion-state-map evil-normal-state-map " ")

(load-theme 'solarized-dark)

(defvar my-packages '(anaphora
		      clojure-mode
		      clojure-project
		      clojure-test-mode
		      elpakit
		      evil
		      evil-leader
		      find-file-in-project
		      ido-ubiquitous
		      idomenu
		      nrepl
		      nzenburn-theme
		      paredit
		      rainbow-delimiters
		      solarized-theme
		      undo-tree))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


(define-key evil-insert-state-map "k" #'cofi/maybe-exit)

(evil-define-command cofi/maybe-exit ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p)))
    (insert "k")
    (let ((evt (read-event (format "Insert %c to exit insert state" ?j)
               nil 0.5)))
      (cond
       ((null evt) (message ""))
       ((and (integerp evt) (char-equal evt ?j))
    (delete-char -1)
    (set-buffer-modified-p modified)
    (push 'escape unread-command-events))
       (t (setq unread-command-events (append unread-command-events
                          (list evt))))))))


(when window-system (set-exec-path-from-shell-PATH))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#002b36" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(custom-enabled-themes (quote (deeper-blue)))
 '(custom-safe-themes (quote ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "4c9ba94db23a0a3dea88ee80f41d9478c151b07cb6640b33bfc38be7c2415cc4" default)))
 '(fci-rule-color "#073642")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-tail-colors (quote (("#073642" . 0) ("#546E00" . 20) ("#00736F" . 30) ("#00629D" . 50) ("#7B6000" . 60) ("#8B2C02" . 70) ("#93115C" . 85) ("#073642" . 100))))
 '(vc-annotate-background "#93a1a1")
 '(vc-annotate-color-map (quote ((20 . "#990A1B") (40 . "#FF6E64") (60 . "#cb4b16") (80 . "#7B6000") (100 . "#b58900") (120 . "#DEB542") (140 . "#546E00") (160 . "#859900") (180 . "#B4C342") (200 . "#3F4D91") (220 . "#6c71c4") (240 . "#9EA0E5") (260 . "#2aa198") (280 . "#69CABF") (300 . "#00629D") (320 . "#268bd2") (340 . "#69B7F0") (360 . "#d33682"))))
 '(vc-annotate-very-old-color "#93115C"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

