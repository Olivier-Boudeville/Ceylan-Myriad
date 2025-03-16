;; This is the fully-integrated Ceylan Emacs configuration, for daily usage.

;; So that our configuration files (which should not be in the
;; user-emacs-directory ~/.emacs.d apparently) can be found:
;;
(add-to-list 'load-path (file-name-concat (getenv "CEYLAN_MYRIAD") "conf"))


;; For all general-purpose basics:
;;
;; (loads it iff it has not been loaded already)
(require 'init-myriad-base)


;; For the Erlang base configuration:
(require 'init-myriad-erlang-base)


;; For the C/C++ base configuration:
(require 'init-myriad-c-cpp-base)


;; For the Python base configuration:
(require 'init-myriad-python-base)

;; To be able to rely on a package manager:
(require 'init-myriad-package-management)


;; For the RST base configuration:
(require 'init-myriad-rst-base)


;; Replaces flymake:
(use-package flycheck
  :ensure t (:wait t) :demand t
  :init (global-flycheck-mode))


;; For more advanced Erlang configuration, requiring packages:
(require 'init-myriad-erlang-advanced)


;; For host-specific settings:
(setq local-conf-file "~/.emacs.d/init-myriad-local.el")

(if (file-exists-p local-conf-file) (require 'init-myriad-local) nil)
