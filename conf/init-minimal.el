;; This is the most minimal Ceylan Emacs configuration.
;;
;; Only very safe, dependency-free settings are enabled here.


;; Not wanting to dismiss the startup screen at each Emacs launch:
(setq inhibit-startup-screen t)

;; Not wanting the rather large Emacs graphical toolbar to be
;; displayed (at the top):
;;
(tool-bar-mode -1)

;; Wanting to follow symbolic links with no specific question:
(setq vc-follow-symlinks t)

;; Displays the line numbers on the left of the editor, in all
;; programming modes (only):
;;
(add-hook 'prog-mode-hook 'display-line-numbers-mode) 


;; Actual key mapping:

;; Not necessary (already built-in):
;; (global-set-key "\C-S" 'isearch-forward)
;; C-x s: save current buffer

(global-set-key "\C-Z" 'undo)
(global-set-key "\C-L" 'goto-line)
