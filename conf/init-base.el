;; This is the base, all-purpose Ceylan Emacs configuration.
;;
;; This is an initialization script written in elisp.
;; Refer to https://www.gnu.org/software/emacs/manual/html_node/elisp/
;;
;; It derives from our main init.el, yet only very safe, context-free
;; settings that do not induce dependencies are enabled here.
;;
;; More involved configurations may include it.


;; Not wanting to dismiss the startup screen at each Emacs launch:
(setq inhibit-startup-screen t)

;; Probably useless then:
(setq inhibit-startup-message t)

;; y/n are shorter:
(defalias 'yes-or-no-p 'y-or-n-p)

;; Not wanting the rather large Emacs graphical toolbar to be
;; displayed (at the top):
;;
;; To avoid "Symbol's function definition is void" on console-based
;; Emacs:
(when (display-graphic-p)
   (tool-bar-mode -1))


;; Wanting to follow symbolic links with no specific question:
(setq vc-follow-symlinks t)

;; Revert automatically buffers when the underlying file has changed,
;; if they do not have unsaved changes:
;;
(global-auto-revert-mode 1)

;; Use the M-x recentf-open-files command to be shown a list of recent
;; files:
;;
;; (not deemed useful enough)
;;(recentf-mode 1)


;; Save previous minibuffer prompts:
(setq history-length 25)
(savehist-mode 1)

;; Remember and restore the last cursor location of opened files:
(save-place-mode 1)

;; Move customization variables to a separate file, to avoid clutter,
;; and load it:
;;
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)


;; Section about autosave and back-up (two different concepts).
;; Taken from http://snarfed.org/space/gnu%20emacs%20backup%20files:

;; Put autosave files (i.e. "#foo#" files) in one (yet per-user, with
;; no common root, to prevent user-related permission issues) place,
;; *not* scattered all over the filesystem!
;;
(defvar autosave-dir
  (concat "/tmp/emacs-myriad-autosaves-" (user-login-name) "/"))

(make-directory autosave-dir t)

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
		  (if buffer-file-name
			  (concat "#" (file-name-nondirectory buffer-file-name) "#")
			(expand-file-name
			 (concat "#%" (buffer-name) "#")))))

;; Put backup files (i.e. "foo~" files) in one place too.
;;
;; The backup-directory-alist list contains regexp=>directory mappings;
;; filenames matching a regexp are backed up in the corresponding
;; directory, created if necessary.
;;
(defvar backup-dir (concat "/tmp/emacs_backups/" (user-login-name) "/"))
(setq backup-directory-alist (list (cons "." backup-dir)))




;; No more annoying, unmutable bell (e.g. when reaching buffer bounds):
(setq ring-bell-function 'ignore)

;; To be able to move between windows simply thanks to S-<arrow> (i.e. holding
;; shift, and hitting one of the 4 arrow keys; however the block selection
;; becomes then a bit more cumbersome):
;;
(windmove-default-keybindings)

;; Displays the key bindings that can follow any currently entered
;; incomplete command (a prefix), like C-x [...]:
;;
(which-key-mode)


;; No limit in the buffer list:
(setq buffers-menu-max-size nil)

;; Inserting text while the mark is active causes the selected text to
;; be deleted first (and also deactivates the mark):
;;
(delete-selection-mode 1)

;; The default behavior of the mark and region, in which setting the
;; mark activates it and highlights the region:
;;
(transient-mark-mode t)

;; No dialog box popped up, just a prompt in the minibuffer:
;; (setq use-file-dialog nil)

;; More general:
(setq use-dialog-box nil)


;; Highlights the line containing point:
(global-hl-line-mode 1)

;; Display how and whether parentheses (or other delimiters) match up:
(show-paren-mode t)

;; Better than 'parenthesis:
(setq show-paren-style 'mixed)

;; Default message not wanted:
(setq initial-scratch-message "")

(setq frame-title-format '("%b" (buffer-file-name ": %f")))

(setq initial-major-mode 'text-mode)
(setq default-major-mode 'text-mode)


;; Show line-number in the mode line:
(line-number-mode 1)

;; Show column-number in the mode line:
(column-number-mode 1)

;; Force some commands to move according to logical lines (i.e.,
;; according to the text lines in the buffer, not the visual ones):
;;
(setq line-move-visual nil)

;; Only for older Emacs apparently: '(setq default-tab-width 4)'
(setq-default tab-width 4)
(setq tab-width 4)

(setq scroll-step 1)

(setq-default fill-column 80)

;; Set cursor color:
(set-cursor-color "white")

;; Color for the cursor line:
;;(set-face-background 'highlight "black")
(set-face-background 'highlight "gray19")

;; Turns off any blinking cursor:
(if (fboundp 'blink-cursor-mode)
	(blink-cursor-mode -1))


;; Set mouse color:
(set-mouse-color "white")

;; Set foreground and background colors:
(set-foreground-color "white")

;;(set-background-color "darkblue")
(set-background-color "black")


;; Set highlighting colors for isearch and drag:
;;(set-face-foreground 'highlight "white")

(set-face-foreground 'region "black")
(set-face-background 'region "lightgreen")

;;(set-face-foreground 'secondary-selection "skyblue")
;;(set-face-background 'secondary-selection "darkblue")

(set-face-foreground 'secondary-selection "red")
(set-face-background 'secondary-selection "green")

;; Low brightness of the background preferred:
;;(setq frame-background-mode 'dark)
'(frame-background-mode (quote dark))


;; Hook section:

;; Displays the line numbers on the left of the editor, in all
;; programming modes:
;;
(add-hook 'prog-mode-hook 'display-line-numbers-mode) 

;; Also useful, for the debugging of document generation:
(add-hook 'rst-mode-hook 'display-line-numbers-mode) 



;; Function section:


;; Indenting buffers as a whole:
;; more info: https://www.emacswiki.org/emacs/DeletingWhitespace#h5o-11
(defun indent-whole-buffer ()
  "Indent whole buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
)


(defun kill-full-line ()
  "Kills the current line as a whole, regardless of the current cursor position. It can be then yanked back with M-y."
  (interactive)
  (let ((orig (point)))
	(beginning-of-line)
	(let ((beg (point)))
	  (forward-line 1)
	  (delete-region beg (point)))
	;; If line is shorter than previous line, then just go to end of line:
	(end-of-line)
	(let ((new (point)))
	  (if (< orig new)
		  (goto-char orig))))
)


(defun save-and-close ()
 (interactive)
 (save-buffer)
 (kill-this-buffer)
)


(defun show-assigned-keys ()
  "Shows the current key bindings (not updated yet)"
  (interactive)
  (message "F1        -> save-buffer" )
  (message "F2        -> query-replace" )
  (message "F3        -> query-replace-regexp" )
  (message "F4        -> indent-whole-buffer" )
  (message "F5        -> undo" )
  (message "F6        -> repeat-complex-command" )
  (message "F7        -> goto-line" )
  (message "F8        -> whitespace-cleanup" )
  (message "F9        -> fd-switch-dictionary" )
  (message "Shift-F9  -> (currently not bound)" )
  (message "F10       -> save-buffers-kill-emacs" )
  (message "F11       -> (does nothing)" )
  (message "F12       -> (does nothing)" )
)

;; These defaults allow to test keys:

(defun default-f1 ()
  (interactive)
  (message "Default for F1")
)

(defun default-f2 ()
  (interactive)
  (message "Default for F2")
)

(defun default-f3 ()
  (interactive)
  (message "Default for F3")
)

(defun default-f4 ()
  (interactive)
  (message "Default for F4")
)

(defun default-f5 ()
  (interactive)
  (message "Default for F5")
)

(defun default-f6 ()
  (interactive)
  (message "Default for F6")
)

(defun default-f7 ()
 (interactive)
 (message "Default for F7")
)

(defun default-f8 ()
 (interactive)
 (message "Default for F8")
)

(defun default-f9 ()
 (interactive)
 (message "Default for F9")
)

(defun default-shift-f9 ()
 (interactive)
 (message "Default for Shift-F9")
)

(defun default-f10 ()
 (interactive)
 (message "Default for F10")
)

(defun default-f11 ()
 (interactive)
 (message "Default for F11")
)

(defun default-f12 ()
 (interactive)
 (message "Default for F12")
)


;; Actual key mapping section.
;;
;; Note that they may be redefined by including configurations (which
;; might offer extra features based on extra dependencies).
;;
;; Use M-x describe-key to know to what function a key sequence is bound.

(global-set-key [delete] 'delete-char)

;; Not necessary (already built-in):
;; - already offered by C-s:
;;  * (global-set-key "\C-S" 'isearch-forward)
;;  * (global-set-key "\C-F" 'isearch-forward)
;; - C-x s: save current buffer

;; As C-r already taken:
(global-set-key "\C-D" 'replace-string)

;; Meant to be overriden
(global-set-key "\C-O" 'find-file)

(global-set-key "\C-Q" 'next-error)

(global-set-key "\C-Z" 'undo)
(global-set-key "\C-L" 'goto-line)

;; not available: (global-set-key "\C-P" 'recompile)

;; See windmove:
(global-set-key [M-right] 'next-buffer)
(global-set-key [M-left]  'previous-buffer)

;; Instead of "kill forward to the end of the sentence" (kill-sentence):
(global-set-key "\M-k" 'kill-full-line)





;; Finally, management of all F* keys:

;; Curiously hitting F1 triggers default-f12:
(global-set-key [f1]			  'default-f1)
(global-set-key [XF86New]		  'default-f1)

;; Usable and behaves like expected:
(global-set-key [f2]              'query-replace)
(global-set-key [XF86Reply]       'query-replace)

;; Usable and behaves like expected:
(global-set-key [f3]			  'query-replace-regexp)
(global-set-key [XF86MailForward] 'query-replace-regexp)

;; Usable and behaves like expected:
(global-set-key [f4]			  'indent-whole-buffer)
(global-set-key [XF86Send]		  'indent-whole-buffer)

;; Curiously bound to Undo:
(global-set-key [f5]              'default-f5)
(global-set-key [XF86New]         'default-f5)

;; Curiously bound to repeat-complex-command:
(global-set-key [f6]			  'default-f6)
(global-set-key [XF86New]		  'default-f6)


;; Usable and behaves like expected:
(global-set-key [f7]			  'goto-line)
(global-set-key [print]			  'goto-line)


;; Usable and behaves like expected:
(global-set-key [f8]              'whitespace-cleanup)
(global-set-key [XF86Save]        'whitespace-cleanup)
(global-set-key [XF86AudioNext]   'whitespace-cleanup)


;; Intercepted by Ubuntu:
(global-set-key [f9]			  'default-f9)
(global-set-key [XF86New]		  'default-f9)


;; Usable and behaves like expected:
(global-set-key [(shift f9)]		'default-shift-f9)
(global-set-key [(shift XF86New)]   'default-shift-f9)
(global-set-key [XF86Explorer]      'default-shift-f9)


;; Usable and behaves like expected:
(global-set-key [f10]				'save-buffers-kill-emacs)
(global-set-key [XF86Documents]     'save-buffers-kill-emacs)


;; Not triggered on my keyboard:
(global-set-key [f11]				'default-f11)
(global-set-key [XF86New]			'default-f11)


;; Not triggered when hitting F12, but triggered when hitting F1 on my keyboard:
(global-set-key [f12]               'save-buffer)
(global-set-key [XF86New]           'save-buffer)



;; Does not seem to apply (two buffers still shown in two window panes
;; if two files specified on the command-line):
;;
;;(delete-other-windows)
