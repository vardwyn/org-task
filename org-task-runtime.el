;;; org-task-runtime.el --- Sampling runtime for org-task -*- lexical-binding: t; -*-
;; Copyright (C) 2026 Vardwyn

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides stateless K/T sampling and completion behavior hooks.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-task-core)
(require 'org-task-sampling)

(defvar org-task-default-k)
(defvar org-task-default-temperature)
(defvar org-task-file)
(defvar org-task-include-tag)
(defvar org-task-once-backlog-keyword)
(defvar org-task-once-active-keyword)
(defvar org-task-finalize-keyword)

(defvar org-task-runtime--sample-cache nil
  "Cached sampled id set keyed by file/seed/K/T and file state stamp.")

(defun org-task-runtime--task-file-cache-stamp ()
  "Return cache stamp for current `org-task-file' content state."
  (let* ((task-file (expand-file-name org-task-file))
         (buffer (and (file-readable-p task-file)
                      (find-file-noselect task-file))))
    (when buffer
      (with-current-buffer buffer
        (list (buffer-chars-modified-tick)
              (buffer-modified-p)
              (when (buffer-file-name)
                (file-attribute-modification-time
                 (file-attributes (buffer-file-name)))))))))

(defun org-task-runtime-default-seed (&optional time)
  "Return default sampling seed for TIME and current file seed iterator."
  (let* ((date-seed (org-task-sampling-date-seed (or time (current-time))))
         (iterator (org-task-core-seed-iterator org-task-file)))
    (if (numberp iterator)
        (format "%s:%d" date-seed iterator)
      date-seed)))

(defun org-task-runtime-sample-tasks (&optional k temperature seed)
  "Return sampled tasks for K, TEMPERATURE and SEED.
Defaults: K=`org-task-default-k', TEMPERATURE=`org-task-default-temperature',
SEED=today local date."
  (let* ((tasks (or (org-task-core-collect-tasks) '()))
         (sample-k (min (max 0 (or k org-task-default-k 0)) (length tasks)))
         (sample-temp (or temperature org-task-default-temperature 1.0))
         (sample-seed (or seed (org-task-runtime-default-seed))))
    (org-task-sampling-select tasks sample-k sample-temp sample-seed (length tasks))))

(defun org-task-runtime-clear-sample-cache ()
  "Clear agenda sample cache."
  (setq org-task-runtime--sample-cache nil))

(defun org-task-runtime-reroll-today ()
  "Increment current file seed iterator, clear cache, and return new seed."
  (let* ((current (org-task-core-seed-iterator org-task-file))
         (next (if (numberp current) (1+ current) 0)))
    (org-task-core-set-seed-iterator next org-task-file)
    (org-task-runtime-clear-sample-cache)
    (org-task-runtime-default-seed)))

(defun org-task-runtime--sampled-id-set (k temperature seed)
  "Return hash-table set of sampled ids for K, TEMPERATURE, and SEED."
  (let* ((key (list (file-truename (expand-file-name org-task-file))
                    (or seed (org-task-runtime-default-seed))
                    (or k org-task-default-k)
                    (or temperature org-task-default-temperature)
                    (org-task-runtime--task-file-cache-stamp)))
         (cached (assoc key org-task-runtime--sample-cache)))
    (if cached
        (cdr cached)
      (let ((table (make-hash-table :test #'equal)))
        (dolist (task (org-task-runtime-sample-tasks k temperature seed))
          (puthash (org-task-core-task-id task) t table))
        (push (cons key table) org-task-runtime--sample-cache)
        table))))

(defun org-task-runtime-agenda-skip-non-sampled (&optional k temperature seed)
  "Agenda skip function that keeps only sampled tasks for K/T/SEED."
  (let* ((id-set (org-task-runtime--sampled-id-set k temperature seed))
         (task-id (org-task-runtime--task-id-at-context)))
    (if (and task-id (gethash task-id id-set))
        nil
      (save-excursion
        (or (org-end-of-subtree t t) (point-max))))))

(defun org-task-runtime--task-id-at-context ()
  "Return task `ID' at current Org/Agenda context, or nil."
  (let ((marker (org-task-runtime--context-marker)))
    (when marker
      (with-current-buffer (marker-buffer marker)
        (save-excursion
          (goto-char marker)
          (org-back-to-heading t)
          (org-entry-get nil org-task-core-id-property nil))))))

(defun org-task-runtime--first-open-keyword ()
  "Return the first non-done TODO keyword, or nil when none exists."
  (cl-find-if
   (lambda (keyword)
     (not (member keyword org-done-keywords)))
   org-todo-keywords-1))

(defun org-task-runtime--keyword-equal (left right)
  "Return non-nil when LEFT and RIGHT match as Org keywords."
  (string= (upcase (string-trim (or left "")))
           (upcase (string-trim (or right "")))))

(defun org-task-runtime--resolve-open-keyword (preferred)
  "Return open keyword matching PREFERRED, falling back to defaults."
  (or (cl-find-if
       (lambda (keyword)
         (and (not (member keyword org-done-keywords))
              (org-task-runtime--keyword-equal keyword preferred)))
       org-todo-keywords-1)
      (org-task-runtime--first-open-keyword)))

(defun org-task-runtime--set-todo-state-if-needed (keyword)
  "Set current heading TODO state to KEYWORD when needed."
  (let ((target (org-task-runtime--resolve-open-keyword keyword))
        (current (org-get-todo-state)))
    (when (and target
               (not (org-task-runtime--keyword-equal current target)))
      (org-todo target))))

(defun org-task-runtime--reopen-current-task ()
  "Move current heading to the first open TODO state if available."
  (org-task-runtime--set-todo-state-if-needed (org-task-runtime--first-open-keyword)))

(defun org-task-runtime--set-once-active-state ()
  "Move current heading into configured once-task active state."
  (let ((target (or (org-task-runtime--resolve-open-keyword org-task-once-active-keyword)
                    (org-task-runtime--resolve-open-keyword org-task-once-backlog-keyword))))
    (when target
      (org-task-runtime--set-todo-state-if-needed target))))

(defun org-task-runtime--finalize-state-p (done-state)
  "Return non-nil when DONE-STATE is the configured finalize keyword."
  (org-task-runtime--keyword-equal done-state org-task-finalize-keyword))

(defun org-task-runtime--task-file-truename ()
  "Return normalized `org-task-file' path."
  (file-truename (expand-file-name org-task-file)))

(defun org-task-runtime--marker-in-task-file-p (marker)
  "Return non-nil when MARKER belongs to `org-task-file'."
  (let ((buffer (and (markerp marker) (marker-buffer marker))))
    (and buffer
         (buffer-file-name buffer)
         (string= (file-truename (buffer-file-name buffer))
                  (org-task-runtime--task-file-truename)))))

(defun org-task-runtime--agenda-marker-at-point ()
  "Return source Org marker referenced by current agenda line, if any."
  (or (get-text-property (point) 'org-hd-marker)
      (get-text-property (point) 'org-marker)))

(defun org-task-runtime--context-marker ()
  "Return source heading marker for current Org/Agenda context."
  (cond
   ((derived-mode-p 'org-agenda-mode)
    (let ((marker (org-task-runtime--agenda-marker-at-point)))
      (and (org-task-runtime--marker-in-task-file-p marker) marker)))
   ((and (derived-mode-p 'org-mode)
         (buffer-file-name)
         (string= (file-truename (buffer-file-name))
                  (org-task-runtime--task-file-truename)))
    (save-excursion
      (ignore-errors
        (org-back-to-heading t)
        (point-marker))))
   (t nil)))

(defun org-task-runtime--apply-done-transition (done-state)
  "Apply done transition for current heading using DONE-STATE keyword."
  (let* ((today (org-task-sampling-date-seed (current-time)))
         (is-task (member org-task-include-tag (org-get-tags nil nil)))
         (repeat-mode (org-task-core-repeat-mode-at-point))
         (is-final (org-task-runtime--finalize-state-p done-state))
         (opened-on (org-entry-get nil org-task-core-opened-on-property nil)))
    (when is-task
      (cond
       (is-final
        (org-set-property org-task-core-last-done-property today))
       ((eq repeat-mode 'repeat)
        (org-set-property org-task-core-last-done-property today)
        (org-task-runtime--reopen-current-task))
       (t
        (if opened-on
            (org-set-property org-task-core-last-done-property today)
          (org-set-property org-task-core-opened-on-property today))
        (org-task-runtime--set-once-active-state)))
      (org-task-runtime-clear-sample-cache))))

(defun org-task-runtime-handle-done (done-state)
  "Apply completion behavior for current context and DONE-STATE."
  (let ((marker (org-task-runtime--context-marker)))
    (when (org-task-runtime--marker-in-task-file-p marker)
      (with-current-buffer (marker-buffer marker)
        (save-excursion
          (goto-char marker)
          (org-back-to-heading t)
          (org-task-runtime--apply-done-transition done-state))))))

(provide 'org-task-runtime)

;;; org-task-runtime.el ends here
