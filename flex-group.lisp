;; Copyright (C) 2014 Joram Schrijver
;;
;;  This file is part of stumpwm.
;;
;; stumpwm is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; stumpwm is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, see
;; <http://www.gnu.org/licenses/>.

(in-package #:stumpwm)

(defclass flex-frame ()
  ((parent :type container
           :initarg :parent
           :initform nil
           :accessor parent)
   (children :initarg :children
             :initform nil
             :accessor children)
   (focused :initarg :current
             :initform nil
             :accessor focused)
   (x :initarg :x
      :initform 0
      :accessor x)
   (y :initarg :y
      :initform 0
      :accessor y)
   (width :initarg :width
          :initform 0
          :accessor width)
   (height :initarg :height
           :initform 0
           :accessor height)))

(defclass simple-flex-frame (flex-frame) ())

(defclass flex-head (simple-flex-frame)
  ((head :initarg :head
         :initform nil
         :accessor head)))

(defclass flex-group (group flex-frame) ())

(defclass flex-window (window)
  ((flex-parent :initarg :parent
            :initform nil
            :accessor parent)))

(defun make-flex-head (parent-group head)
  (make-instance 'flex-head
                 :head head
                 :parent parent-group
                 :x (frame-x head)
                 :y (frame-y head)
                 :width (frame-width head)
                 :height (frame-height head)))

;;; Flex API

(defgeneric add (to item))
(defgeneric hide (object))
;; RESIZE is the name of an existing command, so for now it'll be -RESIZE.
(defgeneric -resize (object &key width height))
(defgeneric move (object &key x y))

;;; Basic implementations

(defmethod add ((frame flex-frame) item)
  (if (typep (focused frame) 'flex-frame)
      (add (focused frame) item)
      (progn
        (setf (parent item) frame)
        (push item (children frame))
        (setf (focused frame) item))))

(defmethod add :after ((frame flex-frame) (window flex-window))
  (focus-window window))

(defmethod add :after ((frame simple-flex-frame) item)
  (when (eq frame (parent item))
    (move item :x (x frame) :y (y frame))
    (-resize item :width (width frame) :height (height frame))))

(defmethod hide ((window window))
  (hide-window window))

(defmethod hide ((frame flex-frame))
  (mapc 'hide (children frame)))

(defmethod -resize ((window flex-window) &key width height)
  (with-accessors ((xwin window-xwin) (parent window-parent))
      window
    (xlib:with-state (parent)
      (xlib:with-state (xwin)
        (when width
          (setf (xlib:drawable-width parent) (+ (xlib:drawable-x xwin) width)
                (xlib:drawable-width xwin) width
                (window-width window) width))
        (when height
          (setf (xlib:drawable-height parent) (+ (xlib:drawable-y xwin) height)
                (xlib:drawable-height xwin) height
                (window-height window) height))))))

(defmethod -resize ((frame flex-frame) &key width height)
  (when width (setf (width frame) width))
  (when height (setf (height frame) height)))

(defmethod -resize ((frame simple-flex-frame) &key width height)
  (when (focused frame) (-resize (focused frame) :width width :height height))
  (when (next-method-p) (call-next-method)))

(defmethod move ((window flex-window) &key x y)
  (with-accessors ((xwin window-xwin) (parent window-parent))
      window
    (xlib:with-state (parent)
      (xlib:with-state (xwin)
        (when x (setf (xlib:drawable-x parent) x
                      (window-x window) x))
        (when y (setf (xlib:drawable-y parent) y
                      (window-y window) y))))))

(defmethod move ((frame flex-frame) &key x y)
  (when x (setf (x frame) x))
  (when y (setf (y frame) y)))

(defmethod move ((frame simple-flex-frame) &key x y)
  (when (focused frame) (move (focused frame) :x x :y y))
  (when (next-method-p) (call-next-method)))


;;; Split frames

(defclass split-flex-frame (flex-frame)
  ((split-ratio :initarg :ratio
                :initform 1/2
                :accessor split-ratio)
   (split-direction :initarg :direction
                    :initform :horizontal
                    :accessor split-direction)))

;; TODO: This function is a mess
(defun update-split (frame)
  (destructuring-bind (first-frame second-frame)
      (children frame)
    (move first-frame :x (x frame) :y (y frame))
    (case (split-direction frame)
      (:horizontal
       (let* ((first-height (floor (* (height frame)
                                      (split-ratio frame))))
              (second-height (- (height frame) first-height)))
         (-resize first-frame :width (width frame)
                              :height first-height)
         (move second-frame :x (x frame)
                            :y (+ (y frame) first-height))
         (-resize second-frame :width (width frame)
                               :height second-height)))
      (:vertical
       (let* ((first-width (floor (* (split-ratio frame) (width frame))))
              (second-width (- (width frame) first-width)))
         (-resize first-frame :width first-width
                              :height (height frame))
         (move second-frame :x (+ (x frame) first-width)
                            :y (y frame))
         (-resize second-frame :width second-width
                               :height (height frame)))))))

(defmethod initialize-instance :after ((frame split-flex-frame)
                                       &key &allow-other-keys)
  (unless (children frame)
    (push (make-instance 'simple-flex-frame
                         :parent frame)
          (children frame))
    (push (make-instance 'simple-flex-frame
                         :parent frame)
          (children frame))
    (update-split frame)
    (setf (focused frame) (first (children frame)))))

(defmethod -resize :after ((frame split-flex-frame) &key width height)
  (declare (ignore width height))
  (update-split frame)
  (when (next-method-p) (call-next-method)))

(defmethod move :after ((frame split-flex-frame) &key x y)
  (declare (ignore x y))
  (update-split frame)
  (when (next-method-p) (call-next-method)))


;;; Utilities

(defgeneric outline (object &optional screen))

(defmethod outline ((group flex-group) &optional (screen (current-screen)))
  (let* ((win (screen-frame-window screen)))
    (xlib:with-state (win)
      (setf (xlib:drawable-x win) (screen-x screen)
            (xlib:drawable-y win) (screen-y screen)
            (xlib:drawable-width win) (screen-width screen)
            (xlib:drawable-height win) (screen-height screen))
      (xlib:map-window win)))
  (mapc 'outline (children group)))

(defmethod outline ((frame flex-frame) &optional (screen (current-screen)))
  (let ((win (screen-frame-window screen))
        (gc (screen-frame-outline-gc screen)))
    (xlib:draw-line win gc
                    (x frame) (y frame)
                    (+ (x frame) (width frame))
                    (y frame))
    (xlib:draw-line win gc
                    (x frame) (y frame)
                    (x frame)
                    (+ (y frame) (height frame)))
    (xlib:draw-line win gc
                    (+ (x frame) (width frame))
                    (y frame)
                    (+ (x frame) (width frame))
                    (+ (y frame) (height frame)))
    (xlib:draw-line win gc
                    (x frame)
                    (+ (y frame) (height frame))
                    (+ (x frame) (width frame))
                    (+ (y frame) (height frame))))
  (mapc 'outline (children frame)))

(defmethod outline :after (object &optional screen)
  (reset-frame-indicator-timer))

(defmethod outline ((window flex-window) &optional (screen (current-screen)))
  (declare (ignore screen)))


;;; Group API

(defmethod initialize-instance :after ((group flex-group)
                                       &key &allow-other-keys)
  (let ((heads (copy-heads (group-screen group))))
    (setf (children group)
          (loop for head in heads
                collect (make-flex-head group head)))
    (setf (focused group) (first (children group)))))

(defmethod group-startup ((group flex-group))
  (setf (focused group) (first (children group))))

(defmethod group-add-window ((group flex-group) window &key &allow-other-keys)
  (change-class window 'flex-window :parent (first (children group)))
  (add group window))

(defmethod group-suspend ((group flex-group)))

(defmethod group-current-window ((group flex-group))
  (loop for frame = (focused group) then (focused frame)
        while (typep frame 'flex-frame)
        finally (return frame)))

(defun find-head (group x y)
  (find-if (lambda (child)
             (and (typep child 'flex-head)
                  (< (x child) x (+ (x child) (width child)))
                  (< (y child) y (+ (y child) (height child)))))
           (children group)))

(defun current-flex-head (group)
  (let ((focus (focused group)))
    (if (typep focus 'flex-head)
        focus
        (find-head group (window-x focus) (window-y focus)))))

(defmethod group-current-head ((group flex-group))
  (head (current-flex-head group)))

(defmethod group-resize-request ((group flex-group) window width height))
(defmethod group-move-request ((group flex-group) window x y relative-to))
(defmethod group-raise-request ((group flex-group) window type))

;;; Restore focus

(defun %flex-focus-next (group)
  (if (group-windows group)
      (group-focus-window group (first (group-windows group)))
      (no-focus group nil)))

(defmethod group-delete-window ((group flex-group) window)
  (setf (children (parent window))
        (remove window (children (parent window))))
  (%flex-focus-next group))

(defmethod group-lost-focus ((group flex-group))
  (%flex-focus-next group))

(defmethod group-wake-up ((group flex-group))
  (%flex-focus-next group))

;;;

(defmethod group-indicate-focus ((group flex-group)))

(defmethod group-focus-window ((group flex-group) window)
  (if window
      (loop
        for current = window then parent
        for parent = (parent current)
        until (eq group parent)
        do (setf (focused parent) current)
        finally (focus-window window))))

(defmethod group-button-press ((group flex-group) x y child))
(defmethod group-root-exposure ((group flex-group)))

(defmethod group-add-head ((group flex-group) head)
  (push (make-flex-head group head) (children group)))

(defmethod group-remove-head ((group flex-group) head))
(defmethod group-resize-head ((group flex-group) old-head new-head))
(defmethod group-sync-all-heads ((group flex-group)))
(defmethod group-sync-head ((group flex-group) head))


;;; Window API

(defmethod update-decoration ((window flex-window)))
;(defmethod focus-window ((window flex-window)))
;(defmethod raise-window ((window flex-window)))

(defmethod window-visible-p ((window flex-window))
  (eq window (focused (parent window))))

(defmethod window-sync ((window flex-window) what-changed))

(defmethod window-head ((window flex-window))
  (find-head (window-group window) (window-x window) (window-y window)))
