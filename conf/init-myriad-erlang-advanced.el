;; This is a more advanced configuration of Emacs regarding Erlang than
;; init-myriad-erlang-base.el.
;;
;; It was inspired from
;; https://github.com/erlang-ls/erlang_ls/blob/main/misc/dotemacs.

;; In a nutshell: quite a lot of these advanced features do not seem ready for
;; actual use, at least in our context; they lag, use huge CPU resources,
;; trigger numerous runtime errors ("LSP :: Method not implemented:
;; textDocument/documentSymbol" and all - origin being erlang_ls), report
;; essentially spurious errors (e.g. short of finding proper includes), lack
;; documentation, trigger unsollicited, problematic rebar3-based rebuilds,
;; freeze randomly and do not provide veritably interesting information. For
;; projects relying a lot on parse-transforms, they seem borderly unusable.
;;
;; So we finally selected only the relevant features, which are currently
;; auto-completion and display documentation of element at mouse cursor.



;; Require and enable the Yasnippet templating system:
;;
;; (however its interest could not be established; probably per-language
;; elements shall be added;
;; https://github.com/AndreaCrotti/yasnippet-snippets/tree/master/snippets/erlang-mode
;; does not seem that useful; this templating system is thus currently
;; deactivated)
;;
;;(use-package yasnippet :ensure (:wait t) :demand t)
;;(yas-global-mode t)



;; company-mode section, for auto-completion.
;;
;; Can be deactivated with '(setq lsp-completion-provider :none)'.

;; Default is 0.2; a null one interferes badly with searching:
(setq company-minimum-prefix-length 1
	  company-idle-delay 0.2)

(use-package company :ensure (:wait t) :demand t)
(add-hook 'after-init-hook 'global-company-mode)



;; With a dark theme, preferring a lightgrey background color for lsp-ui-doc
;; windows (and, for a light theme, a darkgrey background):
;;
(defface lsp-ui-doc-background
  '((((background light)) :background "#777777")
	(t :background "#dddddd"))
  "Background color of the documentation.
Only the `background' is used in this face."
  :group 'lsp-ui-doc)


;; Refer to https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/ for
;; a proper (general-purpose) lsp-mode configuration.


;; Diagnostics providers:

;; As flycheck (from init-myriad-fully-integrated.el)
;; should most probably be disabled when using lsp:
;;
;;(remove-hook 'erlang-mode-hook 'flycheck-mode)


;; Same applies to flymake, which reports many spurious issues in our use case
;; (e.g. due to parse-transforms).
;;
;; Yet this has no effect:
;;(remove-hook 'erlang-mode-hook 'flymake-mode)

;; Same:
;;(flymake-mode -1)

;; The way of silencing them all (fly*):
(setq lsp-diagnostics-provider :none)


;; LSP-related section.
;; See ~/Software/erlang_ls/misc/dotemacs for a configuration example.

;; Requires erlang_ls, typically obtained with:
;; cd ~/Software
;; git clone https://github.com/erlang-ls/
;; cd erlang_ls && make && mkdir bin && cd bin
;;   && ln -s ../_build/default/bin/erlang_ls
;; Then add ${HOME}/Software/erlang_ls/bin to your PATH.
;;
;; erlang_ls shall be regularly updated ('git pull && make all')

(use-package lsp-mode :ensure (:wait t) :demand t)


;; The extra text annotating our code source (e.g. "Used XXX times") is
;; displayed is a code lens (here "function-references"; see
;; https://erlang-ls.github.io/articles/tutorial-code-lenses/).
;;
;; We prefer having these "function-references" disabled, this can be set in
;; myriad/conf/erlang_ls.config, to which ~/.config/erlang_ls is supposed to
;; point (as a symbolic link).
;;
;; Another deactivation option is to use '(setq lsp-lens-enable nil)'.

;; Displayed at the top of a frame (e.g. "src > utils > foo.erl"):
(setq lsp-headerline-breadcrumb-enable t)


;; Enable LSP for Erlang files.
;;
;; Was disabled due too many usability concerns; notably, type checking is
;; based on the sources rather than on the generated BEAM files, resulting in a
;; vast number of false positives.
;;
;; Now LSP is very selectively enabled, mostly to:
;;
;; - enjoy, when leaving the mouse cursor on an element, the display of a
;; contextual window describing its type, documentation, etc.
;;
;; - enable auto-completion based not only an Erlang/OTP modules, but also user
;; ones
;;
(add-hook 'erlang-mode-hook #'lsp)

(with-eval-after-load 'lsp-mode
 (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))


;; Customize prefix for key-bindings:
;; (would clash with "Go to line")
;;(setq lsp-keymap-prefix "C-l")


;; Enable logging for lsp-mode:
;;(setq lsp-log-io t)
;;(setq lsp-log-io nil)

;; To select options, see:
;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/


;; No effect:
;;(setq lsp-completion-enable t)


;; LSP UI Package.
;;
;; See https://github.com/emacs-lsp/lsp-ui and
;; https://emacs-lsp.github.io/lsp-ui/
;;
;; Provides "on hover dialogs": when leaving the mouse cursor on an element,
;; displays a contextual window describing its type, documentation, etc.
;;
(use-package lsp-ui :ensure (:wait t) :demand t)


;; Regarding sideline (shows on the current line, generally on the right,
;; information about the symbols, flycheck diagnostics and LSP code actions; at
;; least often displays useless information in our context).

;; Way too much gibberish content:
;;(setq lsp-ui-sideline-enable t)
(setq lsp-ui-sideline-enable nil)

;;(setq lsp-ui-sideline-show-diagnostics t)
(setq lsp-ui-sideline-show-hover t)

(setq lsp-ui-sideline-show-code-actions t)
;;(setq lsp-ui-sideline-show-code-actions nil)

;;(setq lsp-ui-sideline-update-mode 'line)
(setq lsp-ui-sideline-delay 1)
;;(setq lsp-ui-sideline-diagnostic-max-lines 20)



;; Regarding peek (to determine which code calls a given function):
(setq lsp-ui-peek-enable t)
(setq lsp-ui-peek-show-directory t)



;; Regarding lsp-ui-doc:

(setq lsp-ui-doc-enable t)
;;(setq lsp-ui-doc-enable nil)

(setq lsp-ui-doc-position 'top)
;;(setq lsp-ui-doc-position 'bottom)
;;(setq lsp-ui-doc-position 'at-point)

(setq lsp-ui-doc-side 'left)

;; Number of seconds before showing the doc:
(setq lsp-ui-doc-delay 0.2)

;; Emacs (keyboard) cursor:
(setq lsp-ui-doc-show-with-cursor t)

;; Mouse cursor:
(setq lsp-ui-doc-show-with-mouse t)


;; lsp-ui-doc-background previously set above.

(setq lsp-ui-doc-border "orange")


;; There are lsp-ui-imenu options as well.

(setq lsp-modeline-diagnostics-enable t)
(setq lsp-signature-auto-activate t)
(setq lsp-signature-render-documentation t)


;; LSP Origami Mode (for folding ranges):
;; (does not seem to apply to Erlang)
;;
(use-package lsp-origami :ensure (:wait t) :demand t)

(add-hook 'origami-mode-hook #'lsp-origami-mode)
(add-hook 'erlang-mode-hook #'origami-mode)


;; Provide commands to list workspace symbols:
;; - helm-lsp-workspace-symbol
;; - helm-lsp-global-workspace-symbol
(use-package helm-lsp :ensure (:wait t) :demand t)

(use-package lsp-treemacs :ensure (:wait t) :demand t)

;; Always show diagnostics at the bottom, using 1/3 of the available space:
(add-to-list 'display-buffer-alist
			 `(,(rx bos "*Flycheck errors*" eos)
			  (display-buffer-reuse-window
			   display-buffer-in-side-window)
			  (side            . bottom)
			  (reusable-frames . visible)
			  (window-height   . 0.33)))


;; So that it can be loaded with 'require':
(provide 'init-myriad-erlang-advanced)
