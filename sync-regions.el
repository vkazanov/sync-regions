;;; sync-regions.el --- Sync text and properties between buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Vladimir Kazanov

;; Author: Vladimir Kazanov <vekazanov@gmail.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'cl-lib)

(defvar sync-regions-alist nil
  "A list of `sync-region' objects representing active
synchronization connections.")

(cl-defstruct sync-region
  source-buf
  source-beg-marker
  source-end-marker
  target-buf
  target-beg-marker
  target-end-marker)

(defun sync-regions--copy-text (source-buf source-beg source-end target-buf target-beg target-end)
  "Copy text and properties from SOURCE-BEG to SOURCE-END in
SOURCE-BUF to TARGET-BEG to TARGET-END in TARGET-BUF."
  (let ((text (with-current-buffer source-buf
                (buffer-substring source-beg source-end))))
    (with-current-buffer target-buf
      (save-excursion
        (goto-char target-beg)
        (delete-region target-beg target-end)
        (insert text)))))

(defun sync-regions--apply-changes (beg end len)
  "Sync changes from source to target when changes occur in the
source buffer."
  (let ((change-buf (current-buffer)))
    (dolist (region sync-regions-alist)
      (let ((source-buf (sync-region-source-buf region))
            (source-beg-marker (sync-region-source-beg-marker region))
            (source-end-marker (sync-region-source-end-marker region))
            (target-buf (sync-region-target-buf region))
            (target-beg-marker (sync-region-target-beg-marker region))
            (target-end-marker (sync-region-target-end-marker region)))
        (when (and (eq change-buf source-buf)
                   (>= beg (marker-position source-beg-marker))
                   (<= end (marker-position source-end-marker)))
          (let ((source-beg (marker-position source-beg-marker))
                (source-end (marker-position source-end-marker))
                (target-beg (with-current-buffer target-buf (marker-position target-beg-marker)))
                (target-end (with-current-buffer target-buf (marker-position target-end-marker))))
            (sync-regions--copy-text source-buf source-beg source-end
                                     target-buf target-beg target-end)))))))

(defun sync-regions (source-buf &optional source-beg source-end target-buf target-beg target-end initialize)
  "Start unidirectional syncing from SOURCE-BUF to TARGET-BUF.

If SOURCE-BEG and SOURCE-END are unspecified, use the entire source buffer.
If TARGET-BEG and TARGET-END are unspecified, use the entire target buffer.
If INITIALIZE is non-nil, copy the initial content from the source region to the target region.

Returns a `sync-region' object that can be used to unsync the regions."
  (unless source-buf (setq source-buf (current-buffer)))
  (unless target-buf (error "Target buffer must be specified"))

  ;; Default to entire buffer if regions are unspecified
  (with-current-buffer source-buf
    (unless source-beg (setq source-beg (point-min)))
    (unless source-end (setq source-end (point-max))))
  (with-current-buffer target-buf
    (unless target-beg (setq target-beg (point-min)))
    (unless target-end (setq target-end (point-max))))

  ;; Create markers for regions
  (let ((source-beg-marker (with-current-buffer source-buf (copy-marker source-beg)))
        (source-end-marker (with-current-buffer source-buf (copy-marker source-end t))) ; t for end-marker
        (target-beg-marker (with-current-buffer target-buf (copy-marker target-beg)))
        (target-end-marker (with-current-buffer target-buf (copy-marker target-end t)))
        sync-obj)
    ;; Optionally initialize the target region with the source region's content
    (when initialize
      (sync-regions--copy-text source-buf source-beg source-end target-buf target-beg target-end))
    ;; Create the sync-region object
    (setq sync-obj (make-sync-region :source-buf source-buf
                                     :source-beg-marker source-beg-marker
                                     :source-end-marker source-end-marker
                                     :target-buf target-buf
                                     :target-beg-marker target-beg-marker
                                     :target-end-marker target-end-marker))
    ;; Add to the list and set up hooks
    (push sync-obj sync-regions-alist)
    (with-current-buffer source-buf
      (add-hook 'after-change-functions 'sync-regions--apply-changes nil t))
    sync-obj))

(defun unsync-regions (sync-obj)
  "Stop syncing regions associated with SYNC-OBJ."
  (setq sync-regions-alist (delq sync-obj sync-regions-alist))
  (let ((source-buf (sync-region-source-buf sync-obj)))
    ;; Remove hooks if no more regions to sync in the source buffer
    (unless (cl-some (lambda (region) (eq (sync-region-source-buf region) source-buf))
                     sync-regions-alist)
      (with-current-buffer source-buf
        (remove-hook 'after-change-functions 'sync-regions--apply-changes t)))))

(defun unsync-all-regions ()
  "Stop syncing all regions and remove associated hooks."
  (dolist (sync-obj sync-regions-alist)
    (let ((source-buf (sync-region-source-buf sync-obj)))
      (when (buffer-live-p source-buf)
        (with-current-buffer source-buf
          (remove-hook 'after-change-functions 'sync-regions--apply-changes t)))))
  (setq sync-regions-alist nil))

;;; Test code:

;; ;; Create two buffers
;; (let ((source-buf (get-buffer-create "*source*"))
;;       (target-buf (get-buffer-create "*target*")))
;;   ;; Insert some initial text into the source buffer
;;   (with-current-buffer source-buf
;;     (erase-buffer)
;;     (insert "This is the source buffer.\n"))

;;   ;; Insert different initial text into the target buffer
;;   (with-current-buffer target-buf
;;     (erase-buffer)
;;     (insert "This is the target buffer.\n"))

;;   ;; Display the buffers side by side
;;   (split-window-right)
;;   (other-window 1)
;;   (switch-to-buffer target-buf)
;;   (other-window -1)
;;   (switch-to-buffer source-buf)

;;   ;; Start syncing the entire buffers from source to target, initializing the target with source content
;;   (setq my-sync (sync-regions source-buf nil nil target-buf nil nil t))

;;   ;; Now, any changes made in source-buf will be reflected in target-buf.

;;   ;; To stop syncing:
;;   ;; (unsync-regions my-sync)
;;   )



(provide 'sync-regions)
