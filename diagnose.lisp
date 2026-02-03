;;; Gilt Diagnostic Script
;;; Run with: sbcl --load diagnose.lisp
;;;
;;; This helps diagnose why gilt might freeze on startup

(format t "~%=== Gilt Diagnostic Tool ===~%~%")

;; Check 1: SBCL version
(format t "1. SBCL Version: ~A~%" (lisp-implementation-version))

;; Check 2: /dev/tty access
(format t "~%2. Checking /dev/tty access...~%")
(handler-case
    (let ((tty (open "/dev/tty" :direction :input :element-type '(unsigned-byte 8))))
      (format t "   OK: /dev/tty opened successfully~%")
      (close tty))
  (error (e)
    (format t "   ERROR: Cannot open /dev/tty: ~A~%" e)
    (format t "   This is likely the cause of the freeze!~%")))

;; Check 3: stty command
(format t "~%3. Checking stty command...~%")
(handler-case
    (let ((output (with-output-to-string (s)
                    (sb-ext:run-program "/bin/stty" '("size")
                                        :input t
                                        :output s
                                        :error nil))))
      (format t "   Terminal size: ~A" output))
  (error (e)
    (format t "   ERROR: stty failed: ~A~%" e)))

;; Check 4: Raw mode test
(format t "~%4. Testing raw mode (will restore after)...~%")
(handler-case
    (progn
      (sb-ext:run-program "/bin/stty" '("-echo" "raw" "-icanon")
                          :input t :output nil :error nil)
      (format t "   Raw mode enabled OK~%")
      (sb-ext:run-program "/bin/stty" '("echo" "-raw" "icanon")
                          :input t :output nil :error nil)
      (format t "   Raw mode disabled OK~%"))
  (error (e)
    (format t "   ERROR: Raw mode test failed: ~A~%" e)))

;; Check 5: Terminal type
(format t "~%5. Environment:~%")
(format t "   TERM=~A~%" (sb-ext:posix-getenv "TERM"))
(format t "   SHELL=~A~%" (sb-ext:posix-getenv "SHELL"))
(format t "   DISPLAY=~A~%" (or (sb-ext:posix-getenv "DISPLAY") "(not set)"))

;; Check 6: Quick input test
(format t "~%6. Input test (press any key within 3 seconds)...~%")
(finish-output)
(handler-case
    (progn
      (sb-ext:run-program "/bin/stty" '("-echo" "raw" "-icanon")
                          :input t :output nil :error nil)
      (let* ((tty (open "/dev/tty" :direction :input :element-type '(unsigned-byte 8)))
             (start (get-internal-real-time))
             (timeout (* 3 internal-time-units-per-second))
             (got-input nil))
        (loop while (< (- (get-internal-real-time) start) timeout)
              do (when (listen tty)
                   (read-byte tty)
                   (setf got-input t)
                   (return)))
        (close tty)
        (sb-ext:run-program "/bin/stty" '("echo" "-raw" "icanon")
                            :input t :output nil :error nil)
        (if got-input
            (format t "   OK: Received keyboard input~%")
            (format t "   WARNING: No input received (timeout). This may indicate input issues.~%"))))
  (error (e)
    (sb-ext:run-program "/bin/stty" '("echo" "-raw" "icanon")
                        :input t :output nil :error nil)
    (format t "   ERROR: Input test failed: ~A~%" e)))

(format t "~%=== Diagnostic Complete ===~%")
(format t "~%If you see errors above, please report them.~%")
(format t "If all tests pass but gilt still freezes, the issue may be elsewhere.~%~%")

(sb-ext:exit)
