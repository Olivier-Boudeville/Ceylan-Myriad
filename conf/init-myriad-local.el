;; This is an optional, host-local Emacs configuration.
;; It provides an example to customise host-specific elements.


(message "Applying local settings")



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

;; So that it can be loaded with 'require':
(provide 'init-myriad-local)
