;; This is an optional, host-local Emacs configuration.
;; It provides an example to customise host-specific elements.
;;
;; To avoid committing its local, host-specific changes, we recommend executing:
;; 'git update-index --skip-worktree init-myriad-local.el'.


;;(message "Applying local settings")


;; For a normal-resolution screen:
(add-to-list 'default-frame-alist '(font . "Monospace-9"))

;; For some base laptop resolution screen:
;;(add-to-list 'default-frame-alist '(font . "Monospace-10"))

;; For a high-resolution screen:
;;(add-to-list 'default-frame-alist '(font . "Monospace-11"))


;; Defines the default size of the Emacs window ("frame", in Emacs-speak).
;; (units are characters)

;; 85 width would already allow to display correctly even files with
;; 9999 lines, knowing that the leftmost column for line numbers uses
;; some place. Selecting 88 instead to leave some room to the ... sign
;; used to show a block was folded (anyway the 80-limit is shown by
;; background color).
;;
;;add-to-list 'default-frame-alist (cons 'width  90))


;; Depends on the screen height:

;; For some netbook:
;;(add-to-list 'default-frame-alist (cons 'height 36))

;; For some laptop:
;;(add-to-list 'default-frame-alist (cons 'height 53))

;; For a normal screen:
(add-to-list 'default-frame-alist (cons 'height 56))

;; For a larger screen:
;;(add-to-list 'default-frame-alist (cons 'height 124))



;; Setting the proxy information on the command line prior to executing Emacs is
;; another, possibly more reliable, option:
;;
;;(setq url-proxy-services
;;    '(("no_proxy" . "^\\(localhost\\|10.*\\)")
;;      ("https"    . "foobar.org:1030")
;;      ("http"     . "foobar.org:1030")))


;; So that it can be loaded with 'require':
(provide 'init-myriad-local)
