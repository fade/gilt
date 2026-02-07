;;; Gilt Diagnostic Script
;;; Run with: sbcl --load diagnose.lisp
;;;
;;; This helps diagnose why gilt might freeze on startup
;;; Uses direct termios FFI via sb-posix (no stty dependency)

(require :sb-posix)

(format t "~%=== Gilt Diagnostic Tool ===~%~%")

(defun basic-find-tty ()
  "Find available TTY device"
  (let ((candidates '("/dev/tty" "/dev/pts/0" "/dev/console" "/dev/tty0")))
    (loop for path in candidates
          when (ignore-errors (open path :direction :input :if-does-not-exist nil))
          return path
          finally (return "/dev/tty"))))

;; Check 1: SBCL version
(format t "1. SBCL Version: ~A~%" (lisp-implementation-version))
(format t "   Terminal control: FFI (sb-posix termios)~%")

;; Check 2: TTY access
(format t "~%2. Checking TTY access...~%")
(let ((tty-path (basic-find-tty)))
  (handler-case
      (let ((tty (open tty-path :direction :input :element-type '(unsigned-byte 8))))
        (format t "   OK: ~A opened successfully~%" tty-path)
        (close tty))
    (error (e)
      (format t "   ERROR: Cannot open TTY (~A): ~A~%" tty-path e)
      (format t "   This is likely the cause of the freeze!~%"))))

;; Check 3: Terminal size via ioctl
(format t "~%3. Checking terminal size (ioctl TIOCGWINSZ)...~%")
(handler-case
    (sb-alien:with-alien ((buf (sb-alien:array (sb-alien:unsigned 8) 8)))
      (sb-alien:alien-funcall
       (sb-alien:extern-alien "ioctl"
                              (function sb-alien:int sb-alien:int
                                        sb-alien:unsigned-long (* t)))
       (sb-sys:fd-stream-fd sb-sys:*stdin*)
       #x5413
       (sb-alien:addr (sb-alien:deref buf 0)))
      (let ((rows (logior (sb-alien:deref buf 0) (ash (sb-alien:deref buf 1) 8)))
            (cols (logior (sb-alien:deref buf 2) (ash (sb-alien:deref buf 3) 8))))
        (format t "   Terminal size: ~Dx~D~%" cols rows)))
  (error (e)
    (format t "   ERROR: ioctl failed: ~A~%" e)))

;; Check 4: Raw mode test via termios
(format t "~%4. Testing raw mode (termios FFI)...~%")
(let ((orig nil))
  (handler-case
      (let* ((fd (sb-sys:fd-stream-fd sb-sys:*stdin*))
             (saved (sb-posix:tcgetattr fd))
             (raw (sb-posix:tcgetattr fd)))
        (setf orig saved)
        (setf (sb-posix:termios-lflag raw)
              (logand (sb-posix:termios-lflag raw)
                      (lognot (logior sb-posix:echo sb-posix:icanon))))
        (sb-posix:tcsetattr fd sb-posix:tcsaflush raw)
        (format t "   Raw mode enabled OK~%")
        (sb-posix:tcsetattr fd sb-posix:tcsaflush saved)
        (format t "   Raw mode disabled OK~%"))
    (error (e)
      (when orig
        (ignore-errors
          (sb-posix:tcsetattr (sb-sys:fd-stream-fd sb-sys:*stdin*)
                              sb-posix:tcsaflush orig)))
      (format t "   ERROR: Raw mode test failed: ~A~%" e))))

;; Check 5: Environment
(format t "~%5. Environment:~%")
(format t "   TERM=~A~%" (sb-ext:posix-getenv "TERM"))
(format t "   SHELL=~A~%" (sb-ext:posix-getenv "SHELL"))
(format t "   DISPLAY=~A~%" (or (sb-ext:posix-getenv "DISPLAY") "(not set)"))
(format t "   ALACRITTY_SOCKET=~A~%" (or (sb-ext:posix-getenv "ALACRITTY_SOCKET") "(not set)"))

;; Check 6: Quick input test
(format t "~%6. Input test (press any key within 3 seconds)...~%")
(finish-output)
(let ((orig nil))
  (handler-case
      (let* ((fd (sb-sys:fd-stream-fd sb-sys:*stdin*))
             (saved (sb-posix:tcgetattr fd))
             (raw (sb-posix:tcgetattr fd))
             (tty-path (basic-find-tty)))
        (setf orig saved)
        ;; Enable raw mode
        (setf (sb-posix:termios-lflag raw)
              (logand (sb-posix:termios-lflag raw)
                      (lognot (logior sb-posix:echo sb-posix:icanon))))
        (let ((cc (sb-posix:termios-cc raw)))
          (setf (aref cc sb-posix:vmin) 1)
          (setf (aref cc sb-posix:vtime) 0))
        (sb-posix:tcsetattr fd sb-posix:tcsaflush raw)
        ;; Open TTY for input
        (let* ((tty (open tty-path :direction :input :element-type '(unsigned-byte 8)))
               (tty-fd (sb-sys:fd-stream-fd tty))
               (start (get-internal-real-time))
               (timeout (* 3 internal-time-units-per-second))
               (got-input nil))
          (sb-posix:fcntl tty-fd sb-posix:f-setfl
                          (logior (sb-posix:fcntl tty-fd sb-posix:f-getfl)
                                  sb-posix:o-nonblock))
          (loop while (< (- (get-internal-real-time) start) timeout)
                do (handler-case
                       (let ((byte (read-byte tty nil nil)))
                         (when byte
                           (setf got-input t)
                           (return)))
                     (sb-int:simple-stream-error () nil))
                   (sleep 0.01))
          (close tty)
          (sb-posix:tcsetattr fd sb-posix:tcsaflush saved)
          (if got-input
              (format t "   OK: Received keyboard input~%")
              (format t "   WARNING: No input received (timeout). This may indicate input issues.~%"))))
    (error (e)
      (when orig
        (ignore-errors
          (sb-posix:tcsetattr (sb-sys:fd-stream-fd sb-sys:*stdin*)
                              sb-posix:tcsaflush orig)))
      (format t "   ERROR: Input test failed: ~A~%" e))))

(format t "~%=== Diagnostic Complete ===~%")
(format t "~%If you see errors above, please report them.~%")
(format t "If all tests pass but gilt still freezes, the issue may be elsewhere.~%~%")

(sb-ext:exit)
