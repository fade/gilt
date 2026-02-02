(in-package #:gilt.pty)

;;; PTY Process Runner - CLOS-based
;;; Runs subprocesses with pipes for async I/O
;;; Uses sb-ext:run-program with :wait nil for async execution

;;; Process runner class

(defclass process-runner ()
  ((process :initarg :process :accessor runner-process :initform nil
            :documentation "The sb-ext:process object")
   (output-stream :initarg :output-stream :accessor runner-output-stream :initform nil
                  :documentation "Output stream from process")
   (input-stream :initarg :input-stream :accessor runner-input-stream :initform nil
                 :documentation "Input stream to process")
   (output-lines :initarg :output-lines :accessor runner-output-lines :initform nil
                 :documentation "Accumulated output lines")
   (current-line :initarg :current-line :accessor runner-current-line :initform ""
                 :documentation "Current incomplete line being read")
   (finished-p :initarg :finished-p :accessor runner-finished-p :initform nil
               :documentation "Whether the process has finished")
   (exit-code :initarg :exit-code :accessor runner-exit-code :initform nil
              :documentation "Exit code when finished"))
  (:documentation "Manages an async subprocess with pipe I/O"))

(defmethod print-object ((runner process-runner) stream)
  (print-unreadable-object (runner stream :type t)
    (format stream "~:[running~;finished(~D)~]~@[ ~D lines~]"
            (runner-finished-p runner)
            (runner-exit-code runner)
            (length (runner-output-lines runner)))))

(defun make-process-runner ()
  "Create a new process runner"
  (make-instance 'process-runner))

;;; Generic functions

(defgeneric runner-start (runner command &key directory)
  (:documentation "Start a subprocess with the given command"))

(defgeneric runner-poll (runner)
  (:documentation "Poll for new output and check if process finished. Returns t if there's new data."))

(defgeneric runner-send (runner text)
  (:documentation "Send text to the subprocess stdin"))

(defgeneric runner-stop (runner)
  (:documentation "Stop the subprocess and clean up"))

(defgeneric runner-get-output (runner)
  (:documentation "Get all output lines"))

;;; Methods

(defmethod runner-start ((runner process-runner) command &key directory)
  "Start a subprocess with pipes for I/O"
  (let* ((args (if (listp command) command (list "/bin/sh" "-c" command)))
         (proc (sb-ext:run-program (first args) (rest args)
                                   :input :stream
                                   :output :stream
                                   :error :output  ; Merge stderr into stdout
                                   :wait nil
                                   :directory directory
                                   :search t)))
    (setf (runner-process runner) proc)
    (setf (runner-output-stream runner) (sb-ext:process-output proc))
    (setf (runner-input-stream runner) (sb-ext:process-input proc))
    (setf (runner-output-lines runner) nil)
    (setf (runner-current-line runner) "")
    (setf (runner-finished-p runner) nil)
    (setf (runner-exit-code runner) nil)
    runner))

(defmethod runner-poll ((runner process-runner))
  "Poll for output and check process status. Returns t if new data available."
  (when (runner-finished-p runner)
    (return-from runner-poll nil))
  (let ((stream (runner-output-stream runner))
        (new-data nil))
    ;; Read available characters
    (when stream
      (loop while (listen stream) do
        (let ((char (read-char stream nil nil)))
          (when char
            (setf new-data t)
            (cond
              ((char= char #\Newline)
               (push (runner-current-line runner) (runner-output-lines runner))
               (setf (runner-current-line runner) ""))
              ((char= char #\Return)
               ;; Ignore CR
               )
              (t
               (setf (runner-current-line runner)
                     (concatenate 'string (runner-current-line runner) (string char)))))))))
    ;; Check if process has exited
    (when (runner-process runner)
      (let ((status (sb-ext:process-status (runner-process runner))))
        (when (eq status :exited)
          (setf (runner-finished-p runner) t)
          (setf (runner-exit-code runner) (sb-ext:process-exit-code (runner-process runner)))
          ;; Read any remaining output
          (when stream
            (loop for char = (read-char stream nil nil)
                  while char do
                    (cond
                      ((char= char #\Newline)
                       (push (runner-current-line runner) (runner-output-lines runner))
                       (setf (runner-current-line runner) ""))
                      ((char= char #\Return))
                      (t
                       (setf (runner-current-line runner)
                             (concatenate 'string (runner-current-line runner) (string char)))))))
          ;; Push any remaining partial line
          (when (> (length (runner-current-line runner)) 0)
            (push (runner-current-line runner) (runner-output-lines runner))
            (setf (runner-current-line runner) ""))
          ;; Close streams
          (when (runner-output-stream runner)
            (ignore-errors (close (runner-output-stream runner))))
          (when (runner-input-stream runner)
            (ignore-errors (close (runner-input-stream runner)))))))
    new-data))

(defmethod runner-send ((runner process-runner) text)
  "Send text to the subprocess"
  (when (and (runner-input-stream runner) (not (runner-finished-p runner)))
    (write-string text (runner-input-stream runner))
    (force-output (runner-input-stream runner))))

(defmethod runner-stop ((runner process-runner))
  "Stop the subprocess and clean up"
  (when (and (runner-process runner) (not (runner-finished-p runner)))
    (sb-ext:process-kill (runner-process runner) sb-posix:sigterm)
    (sleep 0.1)
    (when (eq (sb-ext:process-status (runner-process runner)) :running)
      (sb-ext:process-kill (runner-process runner) sb-posix:sigkill))
    (sb-ext:process-wait (runner-process runner)))
  ;; Close streams
  (when (runner-output-stream runner)
    (ignore-errors (close (runner-output-stream runner)))
    (setf (runner-output-stream runner) nil))
  (when (runner-input-stream runner)
    (ignore-errors (close (runner-input-stream runner)))
    (setf (runner-input-stream runner) nil))
  (setf (runner-finished-p runner) t))

(defmethod runner-get-output ((runner process-runner))
  "Get output lines in order (oldest first)"
  (nreverse (copy-list (runner-output-lines runner))))
