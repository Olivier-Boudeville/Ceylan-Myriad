;; This is an initialization script written in elisp.
;; Refer to https://www.gnu.org/software/emacs/manual/html_node/elisp/

;; Inspired from https://github.com/erlang-ls/erlang_ls/blob/main/misc/dotemacs
;; This configuration can be compared with the exammple one, e.g.
;; emacs -q -l ~/Software/erlang_ls/misc/dotemacs XXX.erl

(setq package-enable-at-startup nil)


;; For straight.el, which is a replacement for package.el, not use-package. 
;; use-package can be used with either package.el or straight.el.
;;

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; Temporarily added, to silence 'Package cl is deprecated':
(setq byte-compile-warnings '(cl-functions))

(straight-use-package 'use-package)


;; Note: on a new install, or to update an older one, uncomment once
;; (temporarily) the 'package-refresh-contents)' line below.

;; Use our 'update-emacs-modules.sh' script to update the *.el files of
;; interest.
;;
;; (disabled, as now preferring the Emacs 'packages' system)
;;
;;(setq load-path (cons "~/.emacs.d/my-modules" load-path))
;;
;; Disabled in turn, as apparently straight.el is even better.
;;
;;(require 'package)

;;(add-to-list 'package-archives
;;			 '("melpa" . "https://melpa.org/packages/") t)

;;(package-initialize)

;; Not necessarily a good idea, as launching Emacs becomes slow and dependent
;; on the availability of network for the lookup of the package referential:
;;
;;(package-refresh-contents)


;; Utility function that either installs a package (if it is
;; missing) or requires it (if it already installed).
;;
;;(defun package-require (pkg &optional require-name)
;; "Install a package iff it is not already installed."
;;  (when (not (package-installed-p pkg))
;;	(package-install pkg))
;;  (if require-name
;;	  (require require-name)
;;	(require pkg)))

;;(require 'package)

;; Let's include now all base settings from Ceylan:
(setq ceylan-base-file (locate-user-emacs-file "init-base.el"))
(load-file ceylan-base-file) 



;; To avoid having the Warnings buffer be opened with a message like:
;;
;; """
;; Warning (server): Unable to start the Emacs server.
;; There is an existing Emacs server, named "server".
;; To start the server in this Emacs process, stop the existing
;; server or call ‘M-x server-force-delete’ to forcibly disconnect it.
;; """
;;
;; when opening an extraneous emacs:
;;
;; (does not seem to work, though, hence commented-out)
(straight-use-package 'server)
;;(require 'server)
;;(or (server-running-p)
;;	(server-start))

;; Also tried:
;;(load "server")
;;(unless (server-running-p) (server-start))

;; Compiles .el files newer than their .elc counterpart, or not having one:
;; One can also use M-x byte-compile-file to precompile .el files (e.g. linum).
;; Warning: apparently, unless the .elc file is removed, changes will be
;; effective only when having started emacs again *twice*.
;; Now disabled, to avoid spurious errors, like load-path not being then found,
;; or complaints about free variables:
;;(byte-recompile-directory "~/.emacs.d" 0)

;;(package-require 'flycheck)

;;(add-hook 'after-init-hook #'global-flycheck-mode)


;; RST files support section.

;; May be disabled if slowing emacs down way too much:
(straight-use-package 'rst)

(setq auto-mode-alist
	  (append '(("\\.txt$"  . rst-mode)
				("\\.rst$"  . rst-mode)
				("\\.rst.template$"  . rst-mode)
				("\\.rest$" . rst-mode)) auto-mode-alist))


;; Automatically update the table of contents everytime you adjust a
;; section title:
(add-hook 'rst-adjust-hook 'rst-toc-update)


;; Corresponds to conventions in demo-for-css-testing.rst:
;; (not correctly applied apparently, though)
(setq rst-preferred-adornments'( (?= over-and-under 0)
				 (?- over-and-under 0)
				 (?= simple 0)
				 (?- simple 0)
				 (?. simple 0)
				 (?_ simple 0)
				 (?* simple 0)
				 (?: simple 0)
				 (?+ simple 0) ))



;; Erlang support:

;; Adapted from the README distributed with the OTP tarballs:

;; Note: 'emacs' is here a symbolic link typically created by our
;; install-erlang.sh script, so that upgrading Erlang does not risk to make this
;; directory (e.g. lib/tools-2.8.2/emacs) vanish because of a change in the
;; 'tools' version (thus requiring the current file to be endlessly modified)

;; Two possible conventional locations for an Erlang install:
;;  - either in the user account (in ~/Software/Erlang/Erlang-current-install)
;;  - or directly in the system tree (in /usr/local/lib/erlang/)

(setq load-path (cons "~/Software/Erlang/Erlang-current-install/lib/erlang/emacs" load-path))
(setq load-path (cons "/usr/local/lib/erlang/emacs" load-path))

;;(setq erlang-root-dir "~/Software/Erlang/Erlang-current-install/lib/erlang")
;;(setq erlang-root-dir "/usr/local/lib/erlang")

(setq exec-path (cons "~/Software/Erlang/Erlang-current-install/lib/erlang/bin" exec-path))
(setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))


;; Only in some language modes, not all text modes nor even all programming
;; modes where it is more of a nuisance:
;;
;;(add-hook 'text-mode-hook 'turn-on-auto-fill)
;;(add-hook 'prog-mode-hook 'turn-on-auto-fill)
;; Not so useful even here:
;;(add-hook 'erlang-mode-hook 'turn-on-auto-fill)

;;(require 'erlang-start)


;; Not wanting single '%' to be set at the default column 48:
(add-hook 'erlang-mode-hook (lambda () (setq-local comment-column 0)))

;; erlang-electric-semicolon removed, as more a nuisance than a help (function
;; headers generally pasted from first):
;;
;;(setq erlang-electric-commands '(erlang-electric-comma
;;								 erlang-electric-g))

;; Install the official Erlang mode:
(straight-use-package 'erlang)

;; erlang-electric-semicolon removed, as more a nuisance than a help (function
;; headers generally pasted from first):
;;
;;(setq erlang-electric-commands '(erlang-electric-comma
;;								 erlang-electric-g))



;; Section to show line and column numbers on the left border:
;; more info: https://www.emacswiki.org/emacs/LineNumbers

;; (obsolete now; see also 'longlines')
;;(require 'linum)
;;(add-hook 'find-file-hook (lambda () (linum-mode 1)))

;;(add-hook 'erlang-mode-hook 'linum-mode)
;;(add-hook 'erlang-mode-hook 'column-number-mode)


;; Now:
;;(straight-use-package 'display-line-numbers)

;; List of a major modes on which to disable line numbers:
;;(defcustom display-line-numbers-exempt-modes
;;  ;; Finally not including 'rst-mode' here, as useful for the debugging of
;;  ;; document generation:
;;  ;;
;;  '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode)
;;  "Major modes :."
;;  :group 'display-line-numbers
;;  :type 'list
;;  :version "green")


;;(defun display-line-numbers--turn-on ()
;;  "Turn on line numbers except for certain major modes.
;;Exempt major modes are defined in `display-line-numbers-exempt-modes'."
;;  (unless (or (minibufferp)
;;			  (member major-mode display-line-numbers-exempt-modes))
;;	(display-line-numbers-mode)))

;;(global-display-line-numbers-mode)

;; Best approach done in init-base.erl.


;; LSP-related section
;; See ~/Software/erlang_ls/misc/dotemacs for a configuration example.

;; Requires erlang_ls, typically obtained with:
;; cd ~/Software
;; git clone https://github.com/erlang-ls/
;; cd erlang_ls && make && mkdir bin && cd bin
;;   && ln -s ../_build/default/bin/erlang_ls
;; Then add ${HOME}/Software/erlang_ls/bin to your PATH.

;; Include the Language Server Protocol Clients:
;;(package-require 'lsp-mode)

;; Customize prefix for key-bindings:
;; (would clash with "Go to line")
;;(setq lsp-keymap-prefix "C-l")

;; Enable LSP for Erlang files:
;;
;; (disabled due too many usability concerns)
;;
;;(add-hook 'erlang-mode-hook #'lsp)

;; Require and enable the Yasnippet templating system:
;;(package-require 'yasnippet)
;;(yas-global-mode t)

;; Enable logging for lsp-mode:
;;(setq lsp-log-io t)
;;(setq lsp-log-io nil)

;; To select options, see:
;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/

;; Enable and configure the LSP UI Package:
;; See https://github.com/emacs-lsp/lsp-ui
;;(package-require 'lsp-ui)
;;(use-package lsp-ui)

;; Regarding sideline:
;;(setq lsp-ui-sideline-show-diagnostics t)
;;(setq lsp-ui-sideline-show-hover t)

;;(setq lsp-ui-sideline-show-code-actions t)
;;(setq lsp-ui-sideline-show-code-actions nil)

;;(setq lsp-ui-sideline-update-mode 'line)
;;(setq lsp-ui-sideline-delay ...

;;(setq lsp-ui-sideline-enable t)


;; Regarding peek:
;;(setq lsp-ui-peek-enable t)
;;(setq lsp-ui-peek-show-directory t)


;; Regarding lsp-ui-doc:
;;(setq lsp-ui-doc-enable t)
;;(setq lsp-ui-doc-position 'bottom)
;;(setq lsp-ui-doc-delay Number of seconds before showing the doc...
;;(setq lsp-ui-doc-show-with-cursor t)
;;(setq lsp-ui-doc-show-with-mouse t)


;; Enable LSP Origami Mode (for folding ranges):
;;(package-require 'lsp-origami)
;;(add-hook 'origami-mode-hook #'lsp-origami-mode)
;;(add-hook 'erlang-mode-hook #'origami-mode)

;; Provide commands for type completion, to list workspace symbols:
;; - helm-lsp-workspace-symbol
;; - helm-lsp-global-workspace-symbol
;;(package-install 'helm-lsp)

;;(add-hook 'after-init-hook 'global-company-mode)

;;(setq company-minimum-prefix-length 1
;;	  company-idle-delay 0.0) ;; default is 0.2

;; Which-key integration (now built-in since Emacs 30):
;;(straight-use-package 'which-key)


;;(with-eval-after-load 'lsp-mode
;; (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

;; Always show diagnostics at the bottom, using 1/3 of the available space:
;;(add-to-list 'display-buffer-alist
;;			 `(,(rx bos "*LSP errors*" eos)
;;			  (display-buffer-reuse-window
;;			   display-buffer-in-side-window)
;;			  (side            . bottom)
;;			  (reusable-frames . visible)
;;			  (window-height   . 0.33)))



;; Taken from
;; https://stackoverflow.com/questions/11043004/emacs-compile-buffer-auto-close:

(defun bury-compile-buffer-if-successful (buffer string)
 "Bury a compilation buffer if succeeded without warnings "
 (when (and
		 (buffer-live-p buffer)
		 (string-match "compilation" (buffer-name buffer))
		 (string-match "finished" string)
		 (not
		  (with-current-buffer buffer
			(goto-char (point-min))
			(search-forward "warning" nil t))))
	(run-with-timer 1 nil
					(lambda (buf)
					  (bury-buffer buf)
					  (switch-to-prev-buffer (get-buffer-window buf) 'kill))
					buffer)))

;;(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)


(setq auto-mode-alist
	  (append '(("\\.escript$"  . erlang-mode)) auto-mode-alist))

;;(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)

;; Allows to have Emacs automatically insert newlines to word-wrap:
;; (see https://www.emacswiki.org/emacs/AutoFillMode)
;;(defun my-erlang-mode-hook () (turn-on-auto-fill) )

(message "<<<<<<######### init.el version 1.0 #########>>>>>>")

;; Indentation:
;; Starting from its second line, a multi-line statement should be
;; indented of 2 characters from the beginning of line, not relatively
;; to, say, the opening parenthesis which can be close to the right edge
;; of the line.
(setq c-offsets-alist '(
		;; Otherwise parameters are aligned with the first, whereas we want a
		;; fixed offset:
		(arglist-cont-nonempty . 2)
		(arglist-intro . 2)))


;; Support for C-like languages:
;; (customizations for all of c-mode, c++-mode, objc-mode, java-mode)
(defun my-c-mode-common-hook ()
  (setq cc-default-style "bsd")
  (c-set-offset 'substatement-open 0))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'cc-mode-common-hook 'my-c-mode-common-hook)


;; Not working apparently with emacs 22.2.1:
;;(auto-raise-mode t)


;; Moves the cursor across "physical lines":
;; (finally deactivated, as the 'go to end of line' key was leading to the
;; cursor going downward...)
;;(require 'physical-line)
;;(add-hook 'find-file-hooks 'physical-line-mode-without-exception)



;; Automatic indentation while typing:

;; Does not work correctly with inner bullet lists:
;;(setq indent-line-function 'indent-relative-maybe)

;; Just indents by default at the same level when Enter is hit:
;;(add-hook 'find-file-hooks '(lambda ()
;;      (local-set-key (kbd "RET") 'newline-and-indent)))


;; Useful for most programming modes, but disrupts sub-bullet lists in
;; text (e.g. RST) modes (puts them all at the same level):
;; (not defined as a lambda in order to be able to remove it)
(defun set-advanced-ret-behaviour ()
  ;;(message "############ Setting advanced RET behaviour ###########")
  ;;(local-set-key (kbd "RET") 'reindent-then-newline-and-indent)
  (global-set-key (kbd "RET") 'reindent-then-newline-and-indent)
  )


;;(add-hook 'find-file-hooks 'set-advanced-ret-behaviour)



(defun fix-behaviours-for-text-modes ()
  (message "############## Fixing behaviours for text modes ###########")

  ;; Advanced automatic indentation not adapted to text modes:
  (remove-hook 'find-file-hooks 'set-advanced-ret-behaviour)

  ;; This basic indentation is fine with text modes:
  (global-set-key (kbd "RET") 'newline-and-indent)

  ;;Long lines are normal in text modes:
  ;;(remove-hook 'find-file-hook 'highlight-80+-mode)
  ;; Surely an hack, but works great:
  ;;(setq-local whitespace-line-column 9999)

  ;; No 'lines' or 'empty':
  (setq-local whitespace-style '(face
	tabs trailing space-before-tab newline
	indentation space-after-tab))
  )

(add-hook 'text-mode-hook 'fix-behaviours-for-text-modes)



;; Indenting buffers as a whole:
;; more info: https://www.emacswiki.org/emacs/DeletingWhitespace#h5o-11
(defun indent-whole-buffer ()
  "Indent whole buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
)

;;(add-hook 'find-file-hook 'indent-whole-buffer)
;;(add-hook 'find-file-hook 'whitespace-cleanup)




;;(straight-use-package 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-after-kill-buffer-p 1)
(setq uniquify-ignore-buffers-re "^\\*")

(straight-use-package 'whitespace)
(global-whitespace-mode 1)

;;(setq-default show-trailing-whitespace nil)
;;(setq whitespace-style '(space tabs lines-tail trailing empty indentation space-before-tab space-after-tab))
;; Removed: spaces space-mark tab-mark newline-mark
(setq whitespace-style '(face
	tabs trailing lines space-before-tab newline
	indentation empty space-after-tab))
(setq whitespace-line-column 80)

;; We want to see whether we go past column 80:
;; (currently disabled, as provided by the whitespace mode)
;;(require 'highlight-80+)
;;(add-hook 'find-file-hook 'highlight-80+-mode)


;; 85 width would already allow to display correctly even files with
;; 9999 lines, knowing that the leftmost column for line numbers uses
;; some place. Selecting 88 instead to leave some room to the ... sign
;; used to show a block was folded (anyway the 80-limit is shown by
;; background color).
(add-to-list 'default-frame-alist (cons 'width  88))

;; Depends on the screen height:

;; For a netbook or possibly a laptop:
;;(add-to-list 'default-frame-alist (cons 'height 36))

;; For a normal screen:
(add-to-list 'default-frame-alist (cons 'height 56))



(global-set-key "\C-P" 'recompile)

;; Maybe better than F8:
(global-set-key "\C-O" 'whitespace-cleanup)



(global-set-key [delete] 'delete-char)

;;(global-set-key "TAB" 'reindent-then-newline-and-indent)




;; Compilation section.
;; Mostly taken from http://ensiwiki.ensimag.fr/index.php/Dot_Emacs

;; make compile window disappear after successful compilation:
(setq compilation-finish-function
	  (lambda (buf str)
		(if (string-match "*Compilation*" (buffer-name buf))
			(if (string-match "abnormally" str)
				(message "There were errors :-(")
			  ;; No errors, make the compilation window go away in 2 seconds:
			  (run-at-time 2 nil
						   (lambda (buf)
							 (delete-windows-on buf)
							 (bury-buffer buf))
						   buf)
			  (message "No errors :-)")))))

;;my-compile is smarter about how to display the new buffer
(defun display-buffer-by-splitting-largest (buffer force-other-window)
  "Display buffer BUFFER by splitting the largest buffer vertically, except if
  there is already a window for it."
  (or (get-buffer-window buffer)
	  (let ((new-win
			 (with-selected-window (get-largest-window)
			   (split-window-vertically))))
		(set-window-buffer new-win buffer)
		new-win)))

(defun my-compile ()
  "Ad-hoc display of compilation buffer."
  (interactive)
  (let ((display-buffer-function 'display-buffer-by-splitting-largest))
	(call-interactively 'compile)))

;; Misc compilation settings:
(setq-default
 compile-command "make"
 compilation-read-command nil
 compilation-scroll-output 'first-error
 compilation-ask-about-save nil
 compilation-window-height 10
 compilation-skip-threshold 0
 compilation-auto-jump-to-first-error 1)

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(setq font-lock-maximum-size nil)


;;(standard-display-european 1)


;; Spelling section.

;; Hit F9 to toggle english and french dictionaries:


(setq ispell-dictionary "english")
(setq ispell-program-name "aspell")

;; new error: failed to define function flyspell-mode

;;(add-hook 'text-mode-hook 'flyspell-mode)
(dolist (hook '(text-mode-hook))
(add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
 (add-hook hook (lambda () (flyspell-mode -1))))

(defun fd-switch-dictionary()
	  (interactive)
	  (let* ((dic ispell-current-dictionary)
		 (change (if (string= dic "english") "francais" "english")))
		(ispell-change-dictionary change)
		(message "Dictionary switched from %s to %s" dic change)
		))

(global-set-key (kbd "<f9>")                   'fd-switch-dictionary)
(global-set-key (kbd "<XF86AudioLowerVolume>") 'fd-switch-dictionary)


;; Not working apparently:
;;(require 'flyspell-guess)
;;(eval-after-load "flyspell-guess" '(flyspell-insinuate-guess-indicator))



;; Do not consider underscores and dashes as word separators (otherwise
;; mouse-based search changes its selection during search):
;;
;; (probably a bad idea, search patterns seem not to be found when having a
;; prefix)
;;
;;(global-superword-mode 1)


;; For proper mouse search:
;;(require 'acme-search)
;;(global-set-key [(mouse-3)] 'acme-search-forward)
;;(global-set-key [(shift mouse-3)] 'acme-search-backward)

(straight-use-package 'helm)


(server-start)

;; No more question about clients being still there:
;; (must be *after* server-start)
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)


;; '(package-selected-packages '(flycheck))


;; :height 95 for some resolutions:
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 105 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(rst-level-1-face ((t (:background "#00f" :foreground "#fff"))) t)
 '(rst-level-2-face ((t (:background "#00a" :foreground "#ddd"))) t)
 '(rst-level-3-face ((t (:background "#003" :foreground "#bbb"))) t)
 '(rst-level-4-face ((t (:background "#000" :foreground "#999"))) t)
 '(rst-level-5-face ((t (:background "#010" :foreground "#666"))) t)
 '(rst-level-6-face ((t (:background "#020" :foreground "#555"))) t))


;; Does not seem to apply (two buffers still shown in two window panes
;; if two files specified on the command-line):
;;
;;(delete-other-windows)
