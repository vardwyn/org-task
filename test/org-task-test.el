;;; org-task-test.el --- Tests for org-task -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-agenda)
(add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name)))

(require 'org-task)
(require 'org-task-core)
(require 'org-task-sampling)
(require 'org-task-runtime)

(ert-deftest org-task-core-read-number-fallback ()
  (should (= 1.5 (org-task-core-read-number "1.5" 0.0)))
  (should (= 2.0 (org-task-core-read-number "bad" 2.0)))
  (should (= 3.0 (org-task-core-read-number nil 3.0))))

(ert-deftest org-task-core-resolve-tweak-priority-and-explicit ()
  (cl-labels
      ((resolve-tweak (heading &optional tweak-property)
         (with-temp-buffer
           (org-mode)
           (insert heading "\n")
           (goto-char (point-min))
           (when tweak-property
             (org-set-property org-task-core-tweak-property tweak-property))
           (org-task-core--resolve-tweak-at-point))))
    (should (= 2.0 (resolve-tweak "* TODO [#A] High priority")))
    (should (= 0.5 (resolve-tweak "* TODO [#C] Low priority")))
    (should (= 1.0 (resolve-tweak "* TODO [#B] Medium priority")))
    (should (= 1.0 (resolve-tweak "* TODO No priority")))
    (should (= 1.7 (resolve-tweak "* TODO [#A] Explicit tweak wins" "1.7")))))

(ert-deftest org-task-sampling-select-is-deterministic ()
  (let* ((org-task-default-weight 1.0)
         (org-task-beta-tweak 1.0)
         (org-task-age-s0 7.0)
         (org-task-age-alpha 1.0)
         (org-task-age-cap nil)
         (org-task-open-pressure-steps '((0 . 0.0)))
         (org-task-open-s0 7.0)
         (org-task-open-alpha 1.0)
         (org-task-open-cap nil)
         (tasks (list
                 (org-task-core-task-create :id "a" :weight 1.0 :tweak 1.0 :age-days 1.0 :open-days 1.0
                                            :age-s0 7.0 :age-alpha 1.0 :age-cap nil
                                            :open-s0 7.0 :open-alpha 1.0 :open-cap nil)
                 (org-task-core-task-create :id "b" :weight 1.0 :tweak 1.0 :age-days 1.0 :open-days 1.0
                                            :age-s0 7.0 :age-alpha 1.0 :age-cap nil
                                            :open-s0 7.0 :open-alpha 1.0 :open-cap nil)
                 (org-task-core-task-create :id "c" :weight 1.0 :tweak 1.0 :age-days 1.0 :open-days 1.0
                                            :age-s0 7.0 :age-alpha 1.0 :age-cap nil
                                            :open-s0 7.0 :open-alpha 1.0 :open-cap nil)))
         (first (org-task-sampling-select tasks 2 1.0 "2026-02-18"))
         (second (org-task-sampling-select tasks 2 1.0 "2026-02-18")))
    (should (equal (mapcar #'org-task-core-task-id first)
                   (mapcar #'org-task-core-task-id second)))))

(ert-deftest org-task-sampling-select-respects-k ()
  (let* ((org-task-default-weight 1.0)
         (org-task-beta-tweak 1.0)
         (org-task-age-s0 7.0)
         (org-task-age-alpha 1.0)
         (org-task-age-cap nil)
         (org-task-open-pressure-steps '((0 . 0.0)))
         (org-task-open-s0 7.0)
         (org-task-open-alpha 1.0)
         (org-task-open-cap nil)
         (tasks (list
                 (org-task-core-task-create :id "a" :weight 1.0 :tweak 1.0 :age-days 1.0 :open-days 1.0
                                            :age-s0 7.0 :age-alpha 1.0 :age-cap nil
                                            :open-s0 7.0 :open-alpha 1.0 :open-cap nil)
                 (org-task-core-task-create :id "b" :weight 1.0 :tweak 1.0 :age-days 1.0 :open-days 1.0
                                            :age-s0 7.0 :age-alpha 1.0 :age-cap nil
                                            :open-s0 7.0 :open-alpha 1.0 :open-cap nil)))
         (picked (org-task-sampling-select tasks 5 1.0 "2026-02-18")))
    (should (= 2 (length picked)))))

(ert-deftest org-task-core-normalize-repeat-mode ()
  (should (eq 'repeat (org-task-core-normalize-repeat-mode nil)))
  (should (eq 'repeat (org-task-core-normalize-repeat-mode "repeat")))
  (should (eq 'once (org-task-core-normalize-repeat-mode "once")))
  (should (eq 'once (org-task-core-normalize-repeat-mode "one-shot"))))

(ert-deftest org-task-core-seed-iterator-roundtrip ()
  "Roundtrip task file seed read and write
- Create temp .org file with no seed iteratior
- Read seed iterator value - assert it's empty
- write 0, read zero
- write 3, read 3
- ensure buffer has PROPERTY: ORG_TASK_SEED_ITERATOR 3 verbatim"
  (let* ((temp-file (make-temp-file "org-task-seed-iterator" nil ".org"
                                    "#+title: Tasks\n\n* TODO Seed test :task:\n"))
         (buffer (find-file-noselect temp-file)))
    (unwind-protect
        (progn
          (should-not (org-task-core-seed-iterator temp-file))
          (should (= 0 (org-task-core-set-seed-iterator 0 temp-file)))
          (should (= 0 (org-task-core-seed-iterator temp-file)))
          (should (= 3 (org-task-core-set-seed-iterator 3 temp-file)))
          (should (= 3 (org-task-core-seed-iterator temp-file)))
          (with-current-buffer buffer
            (save-excursion
              (goto-char (point-min))
              (should (re-search-forward
                       "^#\\+PROPERTY: ORG_TASK_SEED_ITERATOR 3$"
                       nil
                       t)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-file temp-file))))

(ert-deftest org-task-runtime-default-seed-includes-top-level-iterator ()
  "With empty iterator seed default-seed returns just the date, with iterator seed present it returns date:iterator_seed"
  (let* ((temp-file (make-temp-file "org-task-default-seed" nil ".org"
                                    "#+title: Tasks\n\n* TODO Seed test :task:\n"))
         (org-task-file temp-file))
    (unwind-protect
        (progn
          (should (equal "2020-01-01"
                         (org-task-runtime-default-seed
                          (encode-time 0 0 0 1 1 2020))))
          (org-task-core-set-seed-iterator 2 temp-file)
          (should (equal "2020-01-01:2"
                         (org-task-runtime-default-seed
                          (encode-time 0 0 0 1 1 2020)))))
      (delete-file temp-file))))

(ert-deftest org-task-runtime-sample-tasks-deterministic ()
  (let* ((org-task-default-k 2)
         (org-task-default-temperature 1.0)
         (tasks (list
                 (org-task-core-task-create :id "a" :weight 1.0 :tweak 1.0 :age-days 1.0 :open-days 1.0
                                            :age-s0 7.0 :age-alpha 1.0 :age-cap nil
                                            :open-s0 7.0 :open-alpha 1.0 :open-cap nil)
                 (org-task-core-task-create :id "b" :weight 1.0 :tweak 1.0 :age-days 1.0 :open-days 1.0
                                            :age-s0 7.0 :age-alpha 1.0 :age-cap nil
                                            :open-s0 7.0 :open-alpha 1.0 :open-cap nil)
                 (org-task-core-task-create :id "c" :weight 1.0 :tweak 1.0 :age-days 1.0 :open-days 1.0
                                            :age-s0 7.0 :age-alpha 1.0 :age-cap nil
                                            :open-s0 7.0 :open-alpha 1.0 :open-cap nil))))
    (cl-letf (((symbol-function 'org-task-core-collect-tasks)
               (lambda (&optional _file) tasks)))
      (let ((first (org-task-runtime-sample-tasks 2 1.0 "2026-02-18"))
            (second (org-task-runtime-sample-tasks 2 1.0 "2026-02-18")))
        (should (equal (mapcar #'org-task-core-task-id first)
                       (mapcar #'org-task-core-task-id second)))))))

(ert-deftest org-task-runtime-reroll-today-creates-and-increments-iterator ()
  "Start with no iterator; call reroll-today; first call must create iterator 0;
return seed date:0, clear the runtime sample cache;
the second call must increment iterator to 1, return date:1 and clear the cache again"
  (let* ((temp-file (make-temp-file "org-task-reroll" nil ".org"
                                    "#+title: Tasks\n\n* TODO Seed test :task:\n"))
         (org-task-file temp-file)
         (org-task-runtime--sample-cache '(cached)))
    (unwind-protect
        (progn
          (should (equal "2020-01-01:0"
                         (cl-letf (((symbol-function 'current-time)
                                    (lambda ()
                                      (encode-time 0 0 0 1 1 2020))))
                           (org-task-runtime-reroll-today))))
          (should (= 0 (org-task-core-seed-iterator temp-file)))
          (should-not org-task-runtime--sample-cache)
          (setq org-task-runtime--sample-cache '(cached-again))
          (should (equal "2020-01-01:1"
                         (cl-letf (((symbol-function 'current-time)
                                    (lambda ()
                                      (encode-time 0 0 0 1 1 2020))))
                           (org-task-runtime-reroll-today))))
          (should (= 1 (org-task-core-seed-iterator temp-file)))
          (should-not org-task-runtime--sample-cache))
      (delete-file temp-file))))

(ert-deftest org-task-agenda-block-uses-lazy-skip-function ()
  (let ((org-task-include-tag "task")
        (org-task-file "/tmp/tasks.org")
        (called nil))
    (cl-letf (((symbol-function 'org-task-agenda-skip-non-sampled)
               (lambda (&rest _args)
                 (setq called t)
                 nil)))
      (let* ((block (org-task-agenda-block "Sample" 5 1.0))
             (options (nth 2 block))
             (skip-fn (cadr (assq 'org-agenda-skip-function options))))
        (should-not called)
        (should (functionp skip-fn))))))

(ert-deftest org-task-runtime-skip-resolves-id-from-agenda-marker ()
  (let* ((temp-file (make-temp-file "org-task-skip" nil ".org"
                                    "* TODO Agenda skip source :task:\n:PROPERTIES:\n:ID: test-id-42\n:END:\n"))
         (source-buffer (find-file-noselect temp-file))
         (org-task-file temp-file)
         (sample-set (let ((table (make-hash-table :test #'equal)))
                       (puthash "test-id-42" t table)
                       table)))
    (unwind-protect
        (with-current-buffer source-buffer
          (org-mode)
          (org-set-regexps-and-options)
          (goto-char (point-min))
          (re-search-forward "^\\* " nil t)
          (beginning-of-line)
          (let ((marker (point-marker)))
            (with-temp-buffer
              (org-agenda-mode)
              (insert "Agenda line\n")
              (put-text-property (point-min) (1- (point-max)) 'org-marker marker)
              (goto-char (point-min))
              (cl-letf (((symbol-function 'org-task-runtime--sampled-id-set)
                         (lambda (&rest _args) sample-set)))
                (should-not
                 (org-task-runtime-agenda-skip-non-sampled 5 1.0 "2026-02-18"))))))
      (when (buffer-live-p source-buffer)
        (kill-buffer source-buffer))
      (delete-file temp-file))))

(ert-deftest org-task-runtime-sampled-id-set-invalidates-on-file-stamp-change ()
  (let ((org-task-file "/tmp/org-task-cache-test.org")
        (org-task-default-k 5)
        (org-task-default-temperature 1.0)
        (org-task-runtime--sample-cache nil)
        (stamp 'stamp-a)
        (calls 0))
    (cl-letf (((symbol-function 'org-task-runtime--task-file-cache-stamp)
               (lambda () stamp))
              ((symbol-function 'org-task-runtime-sample-tasks)
               (lambda (&rest _args)
                 (cl-incf calls)
                 (list (org-task-core-task-create :id "task-1")))))
      (org-task-runtime--sampled-id-set 5 1.0 "2026-02-19")
      (org-task-runtime--sampled-id-set 5 1.0 "2026-02-19")
      (should (= calls 1))
      (setq stamp 'stamp-b)
      (org-task-runtime--sampled-id-set 5 1.0 "2026-02-19")
      (should (= calls 2)))))

(ert-deftest org-task-core-open-days-derived-from-opened-on ()
  (let* ((today (current-time))
         (opened (format-time-string "%Y-%m-%d" (time-subtract today (days-to-time 10))))
         (done (format-time-string "%Y-%m-%d" (time-subtract today (days-to-time 1)))))
    (with-temp-buffer
      (insert "#+TODO: TODO STRT | DONE FIN\n* TODO Aging test :task:\n")
      (org-mode)
      (org-set-regexps-and-options)
      (goto-char (point-min))
      (re-search-forward "^\\* " nil t)
      (beginning-of-line)
      (org-set-property org-task-core-id-property "test-id-1")
      (org-set-property org-task-core-opened-on-property opened)
      (org-set-property org-task-core-last-done-property done)
      (let ((task (org-task-core--make-task)))
        (should (> (org-task-core-task-open-days task)
                   (org-task-core-task-age-days task)))))))

(ert-deftest org-task-runtime-once-first-done-opens-task ()
  (let ((today (org-task-sampling-date-seed (current-time)))
        (org-task-once-active-keyword "STRT")
        (org-task-once-backlog-keyword "TODO")
        (org-task-finalize-keyword "FIN"))
    (with-temp-buffer
      (insert "#+TODO: TODO STRT | DONE FIN\n* TODO One-shot :task:\n")
      (org-mode)
      (org-set-regexps-and-options)
      (goto-char (point-min))
      (re-search-forward "^\\* " nil t)
      (beginning-of-line)
      (org-set-property org-task-core-repeat-property "once")
      (org-todo "DONE")
      (org-task-runtime--apply-done-transition "DONE")
      (should (string= "STRT" (org-get-todo-state)))
      (should (string= today (org-entry-get nil org-task-core-opened-on-property nil)))
      (should-not (org-entry-get nil org-task-core-last-done-property nil)))))

(ert-deftest org-task-runtime-once-subsequent-done-keeps-open-date ()
  (let ((today (org-task-sampling-date-seed (current-time)))
        (org-task-once-active-keyword "STRT")
        (org-task-once-backlog-keyword "TODO")
        (org-task-finalize-keyword "FIN"))
    (with-temp-buffer
      (insert "#+TODO: TODO STRT | DONE FIN\n* STRT One-shot open :task:\n")
      (org-mode)
      (org-set-regexps-and-options)
      (goto-char (point-min))
      (re-search-forward "^\\* " nil t)
      (beginning-of-line)
      (org-set-property org-task-core-repeat-property "once")
      (org-set-property org-task-core-opened-on-property "2026-02-01")
      (org-todo "DONE")
      (org-task-runtime--apply-done-transition "DONE")
      (should (string= "STRT" (org-get-todo-state)))
      (should (string= "2026-02-01" (org-entry-get nil org-task-core-opened-on-property nil)))
      (should (string= today (org-entry-get nil org-task-core-last-done-property nil))))))

(ert-deftest org-task-runtime-finalize-closes-once-task ()
  (let ((today (org-task-sampling-date-seed (current-time)))
        (org-task-once-active-keyword "STRT")
        (org-task-once-backlog-keyword "TODO")
        (org-task-finalize-keyword "FIN"))
    (with-temp-buffer
      (insert "#+TODO: TODO STRT | DONE FIN\n* STRT One-shot final :task:\n")
      (org-mode)
      (org-set-regexps-and-options)
      (goto-char (point-min))
      (re-search-forward "^\\* " nil t)
      (beginning-of-line)
      (org-set-property org-task-core-repeat-property "once")
      (org-set-property org-task-core-opened-on-property "2026-02-01")
      (org-todo "FIN")
      (org-task-runtime--apply-done-transition "FIN")
      (should (string= "FIN" (org-get-todo-state)))
      (should (string= "2026-02-01" (org-entry-get nil org-task-core-opened-on-property nil)))
      (should (string= today (org-entry-get nil org-task-core-last-done-property nil))))))

(ert-deftest org-task-runtime-finalize-closes-repeat-task ()
  (let ((today (org-task-sampling-date-seed (current-time)))
        (org-task-finalize-keyword "FIN"))
    (with-temp-buffer
      (insert "#+TODO: TODO STRT | DONE FIN\n* TODO Repeat final :task:\n")
      (org-mode)
      (org-set-regexps-and-options)
      (goto-char (point-min))
      (re-search-forward "^\\* " nil t)
      (beginning-of-line)
      (org-set-property org-task-core-repeat-property "repeat")
      (org-todo "FIN")
      (org-task-runtime--apply-done-transition "FIN")
      (should (string= "FIN" (org-get-todo-state)))
      (should (string= today (org-entry-get nil org-task-core-last-done-property nil))))))

(ert-deftest org-task-runtime-handles-agenda-context ()
  (let* ((temp-file (make-temp-file "org-task-agenda" nil ".org" "#+TODO: TODO STRT | DONE FIN\n* TODO Agenda flow :task:\n"))
         (source-buffer (find-file-noselect temp-file))
         (org-task-file temp-file)
         (org-task-include-tag "task")
         (org-task-once-active-keyword "STRT")
         (org-task-once-backlog-keyword "TODO")
         (org-task-finalize-keyword "FIN")
         (today (org-task-sampling-date-seed (current-time))))
    (unwind-protect
        (with-current-buffer source-buffer
          (org-mode)
          (org-set-regexps-and-options)
          (goto-char (point-min))
          (re-search-forward "^\\* " nil t)
          (beginning-of-line)
          (org-set-property org-task-core-repeat-property "once")
          (org-todo "DONE")
          (let ((marker (point-marker)))
            (with-temp-buffer
              (org-agenda-mode)
              (insert "Agenda line\n")
              (put-text-property (point-min) (1- (point-max)) 'org-marker marker)
              (goto-char (point-min))
              (org-task-runtime-handle-done "DONE")))
          (goto-char (point-min))
          (re-search-forward "^\\* " nil t)
          (beginning-of-line)
          (should (string= "STRT" (org-get-todo-state)))
          (should (string= today (org-entry-get nil org-task-core-opened-on-property nil))))
      (when (buffer-live-p source-buffer)
        (kill-buffer source-buffer))
      (delete-file temp-file))))

;;; org-task-test.el ends here
