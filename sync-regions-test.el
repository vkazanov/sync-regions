;;; sync-regions-test.el --- Tests for sync-regions.el   -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'sync-regions)

(ert-deftest test-sync-entire-buffer ()
  "Test syncing the entire buffer."
  (let ((source-buf (get-buffer-create "*test-source*"))
        (target-buf (get-buffer-create "*test-target*"))
        sync-obj)
    (unwind-protect
        (progn
          ;; Insert initial content
          (with-current-buffer source-buf
            (erase-buffer)
            (insert "Initial content in source buffer.\n"))
          (with-current-buffer target-buf
            (erase-buffer)
            (insert "Initial content in target buffer.\n"))
          ;; Start syncing
          (setq sync-obj (sync-regions source-buf nil nil target-buf nil nil t))
          ;; Modify the source buffer
          (with-current-buffer source-buf
            (goto-char (point-max))
            (insert "More content.\n"))
          ;; Allow Emacs to process hooks
          (sit-for 0.1)
          ;; Check that the target buffer has the same content
          (should (string= (with-current-buffer source-buf (buffer-string))
                           (with-current-buffer target-buf (buffer-string)))))
      ;; Clean up
      (unsync-regions sync-obj)
      (kill-buffer source-buf)
      (kill-buffer target-buf))))

(ert-deftest test-sync-regions ()
  "Test syncing specific regions within buffers."
  (let ((source-buf (get-buffer-create "*test-source*"))
        (target-buf (get-buffer-create "*test-target*"))
        sync-obj source-beg source-end target-beg target-end)
    (unwind-protect
        (progn
          ;; Insert initial content
          (with-current-buffer source-buf
            (erase-buffer)
            (insert "Line 1: Hello World\n")
            (insert "Line 2: Emacs is great\n")
            (insert "Line 3: End of test\n")
            (goto-char (point-min))
            (forward-line 1)
            (setq source-beg (point))
            (forward-line 1)
            (setq source-end (point)))
          (with-current-buffer target-buf
            (erase-buffer)
            (insert "Target buffer initial content\n")
            (goto-char (point-max))
            (setq target-beg (point))
            (insert "\n")
            (setq target-end (point)))
          ;; Start syncing only Line 2
          (setq sync-obj (sync-regions source-buf source-beg source-end
                                       target-buf target-beg target-end t))
          ;; Modify Line 2 in source buffer
          (with-current-buffer source-buf
            (goto-char source-beg)
            (insert "Modified "))
          ;; Allow Emacs to process hooks
          (sit-for 0.1)
          ;; Check that the target buffer has the modified Line 2
          (should (with-current-buffer target-buf
                    (goto-char (point-min))
                    (re-search-forward "Modified Line 2: Emacs is great" nil t)))))
      ;; Clean up
      (unsync-regions sync-obj)
      (kill-buffer source-buf)
      (kill-buffer target-buf)))

(ert-deftest test-unsync-regions ()
  "Test that unsync-regions stops syncing."
  (let ((source-buf (get-buffer-create "*test-source*"))
        (target-buf (get-buffer-create "*test-target*"))
        sync-obj)
    (unwind-protect
        (progn
          ;; Insert initial content
          (with-current-buffer source-buf
            (erase-buffer)
            (insert "Initial content in source buffer.\n"))
          (with-current-buffer target-buf
            (erase-buffer)
            (insert "Initial content in target buffer.\n"))
          ;; Start syncing
          (setq sync-obj (sync-regions source-buf nil nil target-buf nil nil t))
          ;; Modify the source buffer
          (with-current-buffer source-buf
            (goto-char (point-max))
            (insert "More content.\n"))
          ;; Allow Emacs to process hooks
          (sit-for 0.1)
          ;; Check that the target buffer has the same content
          (should (string= (with-current-buffer source-buf (buffer-string))
                           (with-current-buffer target-buf (buffer-string))))
          ;; Unsync
          (unsync-regions sync-obj)
          ;; Modify the source buffer again
          (with-current-buffer source-buf
            (goto-char (point-max))
            (insert "Even more content.\n"))
          ;; Allow Emacs to process hooks
          (sit-for 0.1)
          ;; Check that the target buffer has not changed
          (should-not (string= (with-current-buffer source-buf (buffer-string))
                               (with-current-buffer target-buf (buffer-string)))))
      ;; Clean up
      (unsync-regions sync-obj)
      (kill-buffer source-buf)
      (kill-buffer target-buf))))

(ert-deftest test-sync-text-properties ()
  "Test that text properties are synced."
  (let ((source-buf (get-buffer-create "*test-source*"))
        (target-buf (get-buffer-create "*test-target*"))
        sync-obj)
    (unwind-protect
        (progn
          ;; Insert initial content with properties
          (with-current-buffer source-buf
            (erase-buffer)
            (insert "This is some text with properties."))
          (with-current-buffer target-buf
            (erase-buffer))
          ;; Start syncing
          (setq sync-obj (sync-regions source-buf nil nil target-buf nil nil t))
          ;; Modify the source buffer
          (with-current-buffer source-buf
            (add-text-properties 6 10 '(face bold))
            (goto-char (point-max))
            (insert " More text."))
          ;; Allow Emacs to process hooks
          (sit-for 0.1)
          ;; Check that the target buffer has the same content and properties
          (should (string= (with-current-buffer source-buf (buffer-string))
                           (with-current-buffer target-buf (buffer-string))))
          ;; Check that the text properties are the same
          (with-current-buffer source-buf
            (let ((props (text-properties-at 8)))
              (with-current-buffer target-buf
                (should (equal props (text-properties-at 8)))))))
      ;; Clean up
      (unsync-regions sync-obj)
      (kill-buffer source-buf)
      (kill-buffer target-buf))))

(ert-deftest test-multiple-syncs ()
  "Test syncing multiple regions from the same source buffer."
  (let ((source-buf (get-buffer-create "*test-source*"))
        (target-buf1 (get-buffer-create "*test-target1*"))
        (target-buf2 (get-buffer-create "*test-target2*"))
        sync-obj1 sync-obj2)
    (unwind-protect
        (progn
          ;; Insert initial content
          (with-current-buffer source-buf
            (erase-buffer)
            (insert "Source buffer content.\n"))
          (with-current-buffer target-buf1
            (erase-buffer))
          (with-current-buffer target-buf2
            (erase-buffer))
          ;; Start syncing to both targets
          (setq sync-obj1 (sync-regions source-buf nil nil target-buf1 nil nil t))
          (setq sync-obj2 (sync-regions source-buf nil nil target-buf2 nil nil t))
          ;; Modify the source buffer
          (with-current-buffer source-buf
            (goto-char (point-max))
            (insert "More content.\n"))
          ;; Allow Emacs to process hooks
          (sit-for 0.1)
          ;; Check that both target buffers have the same content as the source
          (should (string= (with-current-buffer source-buf (buffer-string))
                           (with-current-buffer target-buf1 (buffer-string))))
          (should (string= (with-current-buffer source-buf (buffer-string))
                           (with-current-buffer target-buf2 (buffer-string)))))
      ;; Clean up
      (unsync-regions sync-obj1)
      (unsync-regions sync-obj2)
      (kill-buffer source-buf)
      (kill-buffer target-buf1)
      (kill-buffer target-buf2))))

(ert-deftest test-sync-partial-region ()
  "Test that changes outside the synced region do not affect the target."
  (let ((source-buf (get-buffer-create "*test-source*"))
        (target-buf (get-buffer-create "*test-target*"))
        sync-obj source-beg source-end target-beg target-end)
    (unwind-protect
        (progn
          ;; Insert initial content
          (with-current-buffer source-buf
            (erase-buffer)
            (insert "Line 1: Do not sync\n")
            (insert "Line 2: Sync this line\n")
            (insert "Line 3: Do not sync\n")
            (goto-char (point-min))
            (forward-line 1)
            (setq source-beg (point-marker))
            (forward-line 1)
            (setq source-end (point-marker)))
          (with-current-buffer target-buf
            (erase-buffer)
            (insert "Target initial content\n")
            (goto-char (point-max))
            (setq target-beg (point))
            (insert "\n")
            (setq target-end (point)))
          ;; Start syncing Line 2
          (setq sync-obj (sync-regions source-buf source-beg source-end
                                       target-buf target-beg target-end t))
          ;; Modify outside the synced region
          (with-current-buffer source-buf
            (goto-char (point-min))
            (insert "Modified "))
          ;; Modify inside the synced region
          (with-current-buffer source-buf
            (goto-char source-beg)
            (insert "Modified "))
          ;; Allow Emacs to process hooks
          (sit-for 0.1)
          ;; Check that the target buffer reflects only the change in the synced region
          (with-current-buffer target-buf
            (goto-char (point-min))
            (should (re-search-forward "Modified Line 2: Sync this line" nil t))
            ;; Ensure that changes outside the synced region are not present
            (should-not (re-search-forward "Modified Line 1" nil t)))))
      ;; Clean up
      (unsync-regions sync-obj)
      (kill-buffer source-buf)
      (kill-buffer target-buf)))

(ert-deftest test-target-buffer-changes ()
  "Test that changes in the target buffer do not affect the source buffer."
  (let ((source-buf (get-buffer-create "*test-source*"))
        (target-buf (get-buffer-create "*test-target*"))
        sync-obj)
    (unwind-protect
        (progn
          ;; Insert initial content
          (with-current-buffer source-buf
            (erase-buffer)
            (insert "Initial content.\n"))
          (with-current-buffer target-buf
            (erase-buffer))
          ;; Start syncing
          (setq sync-obj (sync-regions source-buf nil nil target-buf nil nil t))
          ;; Modify the target buffer
          (with-current-buffer target-buf
            (goto-char (point-max))
            (insert "Target buffer modification.\n"))
          ;; Modify the source buffer
          (with-current-buffer source-buf
            (goto-char (point-max))
            (insert " Source buffer modification.\n"))
          ;; Allow Emacs to process hooks
          (sit-for 0.1)
          ;; Check that the source buffer does not have the target's changes
          (should-not (with-current-buffer source-buf
                        (string-match-p "Target buffer modification" (buffer-string))))
          ;; Check that the target buffer reflects the source buffer's latest changes
          (should (with-current-buffer target-buf
                    (string-match-p "Source buffer modification" (buffer-string)))))
      ;; Clean up
      (unsync-regions sync-obj)
      (kill-buffer source-buf)
      (kill-buffer target-buf))))

(ert-deftest test-unsync-all-regions ()
  "Test that unsync-all-regions stops all syncing."
  (let ((source-buf1 (get-buffer-create "*test-source1*"))
        (source-buf2 (get-buffer-create "*test-source2*"))
        (target-buf1 (get-buffer-create "*test-target1*"))
        (target-buf2 (get-buffer-create "*test-target2*"))
        sync-obj1 sync-obj2)
    (unwind-protect
        (progn
          ;; Insert initial content
          (with-current-buffer source-buf1
            (erase-buffer)
            (insert "Source buffer 1 content.\n"))
          (with-current-buffer source-buf2
            (erase-buffer)
            (insert "Source buffer 2 content.\n"))
          (with-current-buffer target-buf1
            (erase-buffer))
          (with-current-buffer target-buf2
            (erase-buffer))
          ;; Start syncing both source buffers to their respective targets
          (setq sync-obj1 (sync-regions source-buf1 nil nil target-buf1 nil nil t))
          (setq sync-obj2 (sync-regions source-buf2 nil nil target-buf2 nil nil t))
          ;; Modify the source buffers
          (with-current-buffer source-buf1
            (goto-char (point-max))
            (insert " More content.\n"))
          (with-current-buffer source-buf2
            (goto-char (point-max))
            (insert " More content.\n"))
          ;; Allow Emacs to process hooks
          (sit-for 0.1)
          ;; Check that both target buffers have the same content as their sources
          (should (string= (with-current-buffer source-buf1 (buffer-string))
                           (with-current-buffer target-buf1 (buffer-string))))
          (should (string= (with-current-buffer source-buf2 (buffer-string))
                           (with-current-buffer target-buf2 (buffer-string))))
          ;; Unsync all regions
          (unsync-all-regions)
          ;; Modify the source buffers again
          (with-current-buffer source-buf1
            (goto-char (point-max))
            (insert " Even more content.\n"))
          (with-current-buffer source-buf2
            (goto-char (point-max))
            (insert " Even more content.\n"))
          ;; Allow Emacs to process hooks
          (sit-for 0.1)
          ;; Check that the target buffers have not changed after unsyncing
          (should-not (string= (with-current-buffer source-buf1 (buffer-string))
                               (with-current-buffer target-buf1 (buffer-string))))
          (should-not (string= (with-current-buffer source-buf2 (buffer-string))
                               (with-current-buffer target-buf2 (buffer-string)))))
      ;; Clean up
      (kill-buffer source-buf1)
      (kill-buffer source-buf2)
      (kill-buffer target-buf1)
      (kill-buffer target-buf2))))

(provide 'sync-regions-test)
