(in-package #:gilt.ui)

;;; UI Components - Panels, Boxes, Text Rendering - CLOS-based
;;; LazyGit-style layout with bordered panels

;;; Panel class

(defclass panel ()
  ((x :initarg :x :accessor panel-x :initform 1)
   (y :initarg :y :accessor panel-y :initform 1)
   (width :initarg :width :accessor panel-width :initform 40)
   (height :initarg :height :accessor panel-height :initform 10)
   (title :initarg :title :accessor panel-title :initform nil)
   (items :initarg :items :accessor panel-items :initform nil
          :documentation "List of displayable items")
   (selected :initarg :selected :accessor panel-selected :initform 0
             :documentation "Selected index")
   (scroll-offset :initarg :scroll-offset :accessor panel-scroll-offset :initform 0
                  :documentation "For scrolling long lists")
   (focused :initarg :focused :accessor panel-focused :initform nil
            :documentation "Whether panel has focus"))
  (:documentation "A bordered panel that can display a list of items"))

(defun make-panel (&key (x 1) (y 1) (width 40) (height 10) title items (selected 0) focused)
  (make-instance 'panel :x x :y y :width width :height height
                        :title title :items items :selected selected :focused focused))

;;; Box drawing characters (Unicode)

(defparameter *box-chars*
  '(:top-left #\┌
    :top-right #\┐
    :bottom-left #\└
    :bottom-right #\┘
    :horizontal #\─
    :vertical #\│
    :t-down #\┬
    :t-up #\┴
    :t-right #\├
    :t-left #\┤
    :cross #\┼))

(defun box-char (name)
  (getf *box-chars* name))

;;; Drawing primitives

(defun draw-text (row col text &key max-width)
  "Draw text at position, optionally truncating"
  (cursor-to row col)
  (let ((display-text (if (and max-width (> (length text) max-width))
                          (concatenate 'string (subseq text 0 (- max-width 1)) "…")
                          text)))
    (write-string display-text *terminal-io*)))

(defun draw-horizontal-line (row col width &optional (char (box-char :horizontal)))
  "Draw a horizontal line"
  (cursor-to row col)
  (loop repeat width do (princ char)))

(defun draw-vertical-line (col start-row end-row &optional (char (box-char :vertical)))
  "Draw a vertical line"
  (loop for row from start-row to end-row do
    (cursor-to row col)
    (princ char)))

(defun draw-box (x y width height &key title (focused nil))
  "Draw a box with optional title"
  (let ((x2 (+ x width -1))
        (y2 (+ y height -1))
        (tl (box-char :top-left))
        (tr (box-char :top-right))
        (bl (box-char :bottom-left))
        (br (box-char :bottom-right))
        (hz (box-char :horizontal))
        (vt (box-char :vertical)))
    ;; Set color based on focus - use visible colors
    (if focused
        (fg (color-code :bright-cyan))
        (fg (color-code :white)))
    (finish-output *terminal-io*)
    ;; Top line with corners
    (cursor-to y x)
    (finish-output *terminal-io*)
    (write-char tl *terminal-io*)
    (loop repeat (- width 2) do (write-char hz *terminal-io*))
    (write-char tr *terminal-io*)
    (finish-output *terminal-io*)
    ;; Vertical lines and bottom corners
    (loop for row from (1+ y) below y2 do
      (cursor-to row x)
      (write-char vt *terminal-io*)
      (cursor-to row x2)
      (write-char vt *terminal-io*))
    (finish-output *terminal-io*)
    ;; Bottom line
    (cursor-to y2 x)
    (write-char bl *terminal-io*)
    (loop repeat (- width 2) do (write-char hz *terminal-io*))
    (write-char br *terminal-io*)
    (finish-output *terminal-io*)
    ;; Title
    (when title
      (cursor-to y (+ x 2))
      (if focused
          (progn (bold) (fg (color-code :bright-white)))
          (fg (color-code :white)))
      (format *terminal-io* " ~A " title))
    (reset)
    (finish-output *terminal-io*)))

;;; Panel rendering

(defun panel-visible-items (panel)
  "Return the items visible in the panel's viewport"
  (let* ((content-height (- (panel-height panel) 2))
         (items (panel-items panel))
         (offset (panel-scroll-offset panel)))
    (subseq items 
            offset 
            (min (length items) (+ offset content-height)))))

(defun draw-panel (panel)
  "Draw a complete panel with border and contents"
  (let* ((x (panel-x panel))
         (y (panel-y panel))
         (w (panel-width panel))
         (h (panel-height panel))
         (focused (panel-focused panel))
         (selected (panel-selected panel))
         (offset (panel-scroll-offset panel))
         (content-width (- w 2)))
    ;; Draw border
    (draw-box x y w h :title (panel-title panel) :focused focused)
    ;; Draw items
    (loop for item in (panel-visible-items panel)
          for i from 0
          for row = (+ y 1 i)
          for actual-index = (+ offset i)
          do
             (cursor-to row (+ x 1))
             ;; Clear the line content area
             (loop repeat content-width do (write-char #\Space *terminal-io*))
             (cursor-to row (+ x 1))
             ;; Highlight selected item
             (when (and focused (= actual-index selected))
               (inverse))
             ;; Render item (can be string or structured)
             (let ((text (if (stringp item) item (format nil "~A" item))))
               (draw-text row (+ x 1) text :max-width content-width))
             (reset))
    (finish-output *terminal-io*)))

;;; Panel navigation

(defun panel-select-next (panel)
  "Move selection down"
  (let ((max-idx (1- (length (panel-items panel)))))
    (when (< (panel-selected panel) max-idx)
      (incf (panel-selected panel))
      (panel-scroll-to-selection panel))))

(defun panel-select-prev (panel)
  "Move selection up"
  (when (> (panel-selected panel) 0)
    (decf (panel-selected panel))
    (panel-scroll-to-selection panel)))

(defun panel-scroll-to-selection (panel)
  "Adjust scroll offset to keep selection visible"
  (let* ((selected (panel-selected panel))
         (offset (panel-scroll-offset panel))
         (visible-height (- (panel-height panel) 2)))
    (cond
      ;; Selection above viewport
      ((< selected offset)
       (setf (panel-scroll-offset panel) selected))
      ;; Selection below viewport
      ((>= selected (+ offset visible-height))
       (setf (panel-scroll-offset panel) (- selected visible-height -1))))))

;;; Status bar

(defun draw-status-bar (row width text &key (bg-color :bright-black))
  "Draw a status bar at the bottom"
  (cursor-to row 1)
  (bg (color-code bg-color))
  (fg (color-code :white))
  (write-string text *terminal-io*)
  ;; Fill rest of line
  (loop repeat (- width (length text)) do (write-char #\Space *terminal-io*))
  (reset)
  (finish-output *terminal-io*))

;;; Help bar

(defun draw-help-bar (row width bindings)
  "Draw a help bar showing key bindings. bindings is alist of (key . description)"
  (cursor-to row 1)
  (bg (color-code 236))
  (loop for (key . desc) in bindings
        for first = t then nil
        do
           (unless first (write-string "  " *terminal-io*))
           (fg (color-code :bright-cyan))
           (write-string key *terminal-io*)
           (fg (color-code :white))
           (write-char #\Space *terminal-io*)
           (write-string desc *terminal-io*))
  ;; Fill rest of line
  (clear-to-end)
  (reset)
  (finish-output *terminal-io*))
