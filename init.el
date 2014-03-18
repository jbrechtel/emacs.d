(global-font-lock-mode t)

(windmove-default-keybindings)

(setq use-file-dialog nil)

;; "Y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Highlight regions and add special behaviors to regions.
;; "C-h d transient" for more info
(setq transient-mark-mode t)

;; Display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)


;custom keybindings
(global-set-key "\C-c\C-w" 'backward-kill-word)
(global-set-key (kbd "C-x f") #'ido-find-file)

(add-hook 'projectile-mode-hook
          (lambda () (local-set-key (kbd "C-x C-f") #'projectile-find-file)))

;; Remove unnecessary gui stuff
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode 0))

(blink-cursor-mode 0) ;; no blink

;;semantic-ectag-util.el

(defcustom semantic-ectag-program "/usr/bin/ctags-exuberant"
  "The Exuberent CTags program to use."
  :group 'semantic
  :type 'program)

(require 'package)
(add-to-list 'package-archives 
    '("melpa" .
      "http://melpa.milkbox.net/packages/"))

(package-initialize)
(ido-mode)
(setq ido-enable-flex-matching t)
(setq inhibit-splash-screen t)
(evil-mode 1)


(setq browse-url-browser-function 'browse-url-firefox
      browse-url-new-window-flag  t
      browse-url-firefox-new-window-is-tab t)

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

(defun switch-full-screen ()
    (interactive)
    (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))

(global-set-key [f11] 'switch-full-screen)

(setq-default indent-tabs-mode nil)

(require 'twittering-mode)
(setq twittering-use-master-password t)
;(setq twittering-use-ssl t)
;(setq twittering-oauth-use-ssl t)
;(setq twittering-curl-program "/usr/bin/curl")

; filetypes
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("zshrc" . shell-script-mode))
(add-to-list 'auto-mode-alist '("aliases" . shell-script-mode))

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

(when window-system (set-exec-path-from-shell-PATH))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#002b36" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(background-color "#002b36")
 '(background-mode dark)
 '(cursor-color "#839496")
 '(custom-safe-themes (quote ("968d1ad07c38d02d2e5debffc5638332696ac41af7974ade6f95841359ed73e3" "2b5aa66b7d5be41b18cc67f3286ae664134b95ccc4a86c9339c886dfd736132d" "f0ea6118d1414b24c2e4babdc8e252707727e7b4ff2e791129f240a2b3093e32" "a6df4e244c3ced9e44376747bf70b5499a02212e5f8e40ac68c4a6c5cc6f86d2" "dde1442aa95d2993b0b170610493772145f4ef7b5eb5ae7daef394a874c012c1" default)))
 '(fci-rule-color "#073642")
 '(foreground-color "#839496")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-tail-colors (quote (("#073642" . 0) ("#546E00" . 20) ("#00736F" . 30) ("#00629D" . 50) ("#7B6000" . 60) ("#8B2C02" . 70) ("#93115C" . 85) ("#073642" . 100))))
 '(line-number-mode t)
 '(tab-width 2)
 '(vc-annotate-background "#93a1a1")
 '(vc-annotate-color-map (quote ((20 . "#990A1B") (40 . "#FF6E64") (60 . "#cb4b16") (80 . "#7B6000") (100 . "#b58900") (120 . "#DEB542") (140 . "#546E00") (160 . "#859900") (180 . "#B4C342") (200 . "#3F4D91") (220 . "#6c71c4") (240 . "#9EA0E5") (260 . "#2aa198") (280 . "#69CABF") (300 . "#00629D") (320 . "#268bd2") (340 . "#69B7F0") (360 . "#d33682"))))
 '(vc-annotate-very-old-color "#93115C"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "gray20" :foreground "white smoke" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 145 :width normal :foundry "unknown" :family "monofur")))))

(global-linum-mode 1)
(projectile-global-mode)

(find-file "~/synced/state.org")

(byte-recompile-directory "/home/jbrechtel/.emacs.d")

(load-theme 'cyberpunk)
