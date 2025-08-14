;; This is the fully-integrated Ceylan Emacs configuration, for daily usage.

;; To find the next init-myriad-base.el and all, we put the Myriad configuration
;; directory in **last** position, so that it is used as a last resort (instead
;; of shadowing any file that the user would have put in ~/.emacs.d/).
;;
;; Note that apparently the ~/.emacs.d directory is by default not in the
;; load-path, so, to let the user define overriding files there, we add it as
;; well, but just prior to the aforementioned Myriad configuration directory:

(push "~/.emacs.d" (cdr (last load-path)))
(push (file-name-concat (getenv "CEYLAN_MYRIAD") "conf") (cdr (last load-path)))


; This would add the Myriad directory at beginning, thus having it
;; top-priority, whereas we want it to be the least prioritary:
;; (add-to-list 'load-path (file-name-concat (getenv "CEYLAN_MYRIAD") "conf"))

;;(message "The load path is: %s" load-path)


;; For all general-purpose basics:
;;
;; (loads it iff it has not been loaded already)
;;
;; Apparently 'require' triggers package.el, leading to the following warning
;; in some versions: "Warning (emacs): Package.el loaded before Elpaca".
;;
(require 'init-myriad-base)


;; For the Erlang base configuration:
(require 'init-myriad-erlang-base)


;; For the C/C++ base configuration:
(require 'init-myriad-c-cpp-base)


;; For the Python base configuration:
(require 'init-myriad-python-base)


;;(message "Before myriad-local")

;; Not done later, as the local settings may specify the use of a proxy, which
;; must be setup prior to the package management below:

;; Hence to be found through the original load path, otherwise in ~/.emacs.d,
;; otherwise in Myriad configuration directory:

(setq myriad-conf-file "init-myriad-local.el")

(if (locate-file myriad-conf-file load-path)
    (require 'init-myriad-local)
    (message
	 (concat "(no '" myriad-conf-file "' Myriad local configuration found)")))

;;(message "After myriad-local")



;; To be able to rely on a package manager:
(require 'init-myriad-package-management)


;; For the RST base configuration:
(require 'init-myriad-rst-base)


;; Replaces flymake; for Erlang, triggers rebar3 if such a project is found:
;; (refer to https://www.flycheck.org/en/latest/languages.html#erlang)
;;
;;(use-package flycheck
;;  :ensure t (:wait t) :demand t
;;  :init (global-flycheck-mode))


;; For more advanced Erlang configuration, requiring packages:
(require 'init-myriad-erlang-advanced)



;; So that it can be loaded with 'require':
(provide 'init-myriad-fully-integrated)
