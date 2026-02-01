(in-package #:gilt.views)

;;; Views - Main UI screens
;;; LazyGit-style layout with multiple panels

;;; View base class

(defclass view ()
  ((panels :initarg :panels :accessor view-panels :initform nil)
   (focused-panel :initarg :focused-panel :accessor view-focused-panel :initform 0)
   (needs-refresh :initarg :needs-refresh :accessor view-needs-refresh :initform t)))

(defgeneric draw-view (view width height)
  (:documentation "Draw the view to the terminal"))

(defgeneric handle-key (view key)
  (:documentation "Handle a key event, return :quit to exit, :switch to change view"))

(defgeneric refresh-data (view)
  (:documentation "Refresh data from git"))

;;; Status View - Main view showing files and diff

(defclass status-view (view)
  ((status-panel :accessor status-panel)
   (staged-panel :accessor staged-panel)
   (diff-panel :accessor diff-panel)
   (status-entries :accessor status-entries :initform nil)
   (current-diff :accessor current-diff :initform nil)))

(defmethod initialize-instance :after ((view status-view) &key)
  (setf (status-panel view)
        (make-panel :title "Unstaged Changes" :focused t))
  (setf (staged-panel view)
        (make-panel :title "Staged Changes"))
  (setf (diff-panel view)
        (make-panel :title "Diff"))
  (setf (view-panels view)
        (list (status-panel view) (staged-panel view) (diff-panel view)))
  (refresh-data view))

(defmethod refresh-data ((view status-view))
  (let ((entries (git-status)))
    (setf (status-entries view) entries)
    ;; Split into staged and unstaged
    (setf (panel-items (status-panel view))
          (loop for e in entries
                unless (status-entry-staged-p e)
                  collect (format-status-entry e)))
    (setf (panel-items (staged-panel view))
          (loop for e in entries
                when (status-entry-staged-p e)
                  collect (format-status-entry e)))
    ;; Update diff for selected file
    (update-diff view)))

(defun format-status-entry (entry)
  "Format a status entry for display"
  (format nil "~A ~A"
          (case (status-entry-status entry)
            (:modified "M")
            (:added "A")
            (:deleted "D")
            (:untracked "?")
            (:renamed "R")
            (t " "))
          (status-entry-file entry)))

(defun update-diff (view)
  "Update diff panel based on current selection"
  (let* ((focused-idx (view-focused-panel view))
         (panel (nth focused-idx (view-panels view)))
         (selected (panel-selected panel))
         (entries (if (= focused-idx 0)
                      (remove-if #'status-entry-staged-p (status-entries view))
                      (remove-if-not #'status-entry-staged-p (status-entries view)))))
    (when (and entries (< selected (length entries)))
      (let* ((entry (nth selected entries))
             (file (status-entry-file entry))
             (diff (if (= focused-idx 0)
                       (git-diff file)
                       (git-diff-staged file))))
        (setf (current-diff view) diff)
        (setf (panel-items (diff-panel view))
              (cl-ppcre:split "\\n" diff))))))

(defmethod draw-view ((view status-view) width height)
  ;; Layout: left side has stacked panels, right side has diff
  (let* ((left-width (floor width 3))
         (right-width (- width left-width))
         (top-height (floor (- height 2) 2))
         (bottom-height (- height 2 top-height)))
    ;; Position panels
    (setf (panel-x (status-panel view)) 1
          (panel-y (status-panel view)) 1
          (panel-width (status-panel view)) left-width
          (panel-height (status-panel view)) top-height)
    (setf (panel-x (staged-panel view)) 1
          (panel-y (staged-panel view)) (1+ top-height)
          (panel-width (staged-panel view)) left-width
          (panel-height (staged-panel view)) bottom-height)
    (setf (panel-x (diff-panel view)) (1+ left-width)
          (panel-y (diff-panel view)) 1
          (panel-width (diff-panel view)) right-width
          (panel-height (diff-panel view)) (- height 2))
    ;; Update focus state
    (loop for panel in (view-panels view)
          for i from 0
          do (setf (panel-focused panel) (= i (view-focused-panel view))))
    ;; Draw panels
    (dolist (panel (view-panels view))
      (draw-panel panel))
    ;; Draw help bar
    (draw-help-bar (1- height) width
                   '(("j/k" . "navigate")
                     ("Tab" . "switch panel")
                     ("Space" . "stage/unstage")
                     ("c" . "commit")
                     ("q" . "quit")))))

(defmethod handle-key ((view status-view) key)
  (let* ((focused-idx (view-focused-panel view))
         (panel (nth focused-idx (view-panels view))))
    (cond
      ;; Quit
      ((and (key-event-char key) (char= (key-event-char key) #\q))
       :quit)
      ;; Navigation - down
      ((or (eq (key-event-code key) +key-down+)
           (and (key-event-char key) (char= (key-event-char key) #\j)))
       (panel-select-next panel)
       (update-diff view)
       nil)
      ;; Navigation - up
      ((or (eq (key-event-code key) +key-up+)
           (and (key-event-char key) (char= (key-event-char key) #\k)))
       (panel-select-prev panel)
       (update-diff view)
       nil)
      ;; Switch panel
      ((eq (key-event-code key) +key-tab+)
       (setf (view-focused-panel view)
             (mod (1+ focused-idx) 2))  ; Only switch between status and staged
       (update-diff view)
       nil)
      ;; Stage/unstage with space
      ((and (key-event-char key) (char= (key-event-char key) #\Space))
       (let* ((entries (if (= focused-idx 0)
                           (remove-if #'status-entry-staged-p (status-entries view))
                           (remove-if-not #'status-entry-staged-p (status-entries view))))
              (selected (panel-selected panel)))
         (when (and entries (< selected (length entries)))
           (let ((entry (nth selected entries)))
             (if (= focused-idx 0)
                 (git-stage-file (status-entry-file entry))
                 (git-unstage-file (status-entry-file entry)))
             (refresh-data view))))
       nil)
      ;; Commit
      ((and (key-event-char key) (char= (key-event-char key) #\c))
       :commit)
      ;; Refresh
      ((and (key-event-char key) (char= (key-event-char key) #\r))
       (refresh-data view)
       nil)
      (t nil))))

;;; Log View

(defclass log-view (view)
  ((log-panel :accessor log-panel)
   (log-entries :accessor log-entries :initform nil)))

(defmethod initialize-instance :after ((view log-view) &key)
  (setf (log-panel view)
        (make-panel :title "Commits" :focused t))
  (setf (view-panels view) (list (log-panel view)))
  (refresh-data view))

(defmethod refresh-data ((view log-view))
  (let ((entries (git-log :count 100)))
    (setf (log-entries view) entries)
    (setf (panel-items (log-panel view))
          (loop for e in entries
                collect (format nil "~A ~A ~A"
                                (log-entry-short-hash e)
                                (log-entry-message e)
                                (log-entry-date e))))))

(defmethod draw-view ((view log-view) width height)
  (setf (panel-x (log-panel view)) 1
        (panel-y (log-panel view)) 1
        (panel-width (log-panel view)) width
        (panel-height (log-panel view)) (- height 2)
        (panel-focused (log-panel view)) t)
  (draw-panel (log-panel view))
  (draw-help-bar (1- height) width
                 '(("j/k" . "navigate")
                   ("Enter" . "view")
                   ("1" . "status")
                   ("q" . "quit"))))

(defmethod handle-key ((view log-view) key)
  (let ((panel (log-panel view)))
    (cond
      ((and (key-event-char key) (char= (key-event-char key) #\q))
       :quit)
      ((or (eq (key-event-code key) +key-down+)
           (and (key-event-char key) (char= (key-event-char key) #\j)))
       (panel-select-next panel)
       nil)
      ((or (eq (key-event-code key) +key-up+)
           (and (key-event-char key) (char= (key-event-char key) #\k)))
       (panel-select-prev panel)
       nil)
      ((and (key-event-char key) (char= (key-event-char key) #\1))
       :status)
      (t nil))))

;;; Branches View

(defclass branches-view (view)
  ((branches-panel :accessor branches-panel)
   (branch-list :accessor branch-list :initform nil)
   (current-branch :accessor current-branch :initform nil)))

(defmethod initialize-instance :after ((view branches-view) &key)
  (setf (branches-panel view)
        (make-panel :title "Branches" :focused t))
  (setf (view-panels view) (list (branches-panel view)))
  (refresh-data view))

(defmethod refresh-data ((view branches-view))
  (setf (branch-list view) (git-branches))
  (setf (current-branch view) (git-current-branch))
  (setf (panel-items (branches-panel view))
        (loop for b in (branch-list view)
              collect (if (string= b (current-branch view))
                          (format nil "* ~A" b)
                          (format nil "  ~A" b)))))

(defmethod draw-view ((view branches-view) width height)
  (setf (panel-x (branches-panel view)) 1
        (panel-y (branches-panel view)) 1
        (panel-width (branches-panel view)) width
        (panel-height (branches-panel view)) (- height 2)
        (panel-focused (branches-panel view)) t)
  (draw-panel (branches-panel view))
  (draw-help-bar (1- height) width
                 '(("j/k" . "navigate")
                   ("Enter" . "checkout")
                   ("n" . "new branch")
                   ("1" . "status")
                   ("q" . "quit"))))

(defmethod handle-key ((view branches-view) key)
  (let ((panel (branches-panel view)))
    (cond
      ((and (key-event-char key) (char= (key-event-char key) #\q))
       :quit)
      ((or (eq (key-event-code key) +key-down+)
           (and (key-event-char key) (char= (key-event-char key) #\j)))
       (panel-select-next panel)
       nil)
      ((or (eq (key-event-code key) +key-up+)
           (and (key-event-char key) (char= (key-event-char key) #\k)))
       (panel-select-prev panel)
       nil)
      ((eq (key-event-code key) +key-enter+)
       (let* ((selected (panel-selected panel))
              (branch (nth selected (branch-list view))))
         (when branch
           (git-checkout branch)
           (refresh-data view)))
       nil)
      ((and (key-event-char key) (char= (key-event-char key) #\1))
       :status)
      (t nil))))
