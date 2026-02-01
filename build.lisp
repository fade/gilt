;;; Build script for Gilt executable

(require :asdf)

;; Add project to ASDF search path
(push (truename ".") asdf:*central-registry*)

;; Load the system
(format t "Loading gilt...~%")
(force-output)
(asdf:load-system :gilt)
(format t "System loaded. Building executable...~%")
(force-output)

;; Build the executable
;; Use maximum compression (1-9, where 9 is highest)
(sb-ext:save-lisp-and-die "gilt"
                          :toplevel #'gilt:main
                          :executable t
                          :compression 9)
