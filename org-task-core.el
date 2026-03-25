;;; org-task-core.el --- Core Org parsing for org-task -*- lexical-binding: t; -*-
;;;
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

;; Collects eligible task headings and reads inherited parameters.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-id)
(require 'subr-x)

(defvar org-task-file)
(defvar org-task-include-tag)
(defvar org-task-default-weight)
(defvar org-task-age-s0)
(defvar org-task-age-alpha)
(defvar org-task-age-cap)
(defvar org-task-open-s0)
(defvar org-task-open-alpha)
(defvar org-task-open-cap)

(defconst org-task-core-id-property "ID"
  "Org built-in property name for stable task identity.")

(defconst org-task-core-weight-property "ORG_TASK_WEIGHT"
  "Property name for inherited base weight.")

(defconst org-task-core-tweak-property "ORG_TASK_TWEAK"
  "Property name for per-task tweak multiplier.")

(defconst org-task-core-age-s0-property "ORG_TASK_AGE_S0"
  "Property name for per-task age scale.")

(defconst org-task-core-age-alpha-property "ORG_TASK_AGE_ALPHA"
  "Property name for per-task age exponent.")

(defconst org-task-core-age-cap-property "ORG_TASK_AGE_CAP"
  "Property name for per-task age cap.")

(defconst org-task-core-open-s0-property "ORG_TASK_OPEN_S0"
  "Property name for per-task open-age scale.")

(defconst org-task-core-open-alpha-property "ORG_TASK_OPEN_ALPHA"
  "Property name for per-task open-age exponent.")

(defconst org-task-core-open-cap-property "ORG_TASK_OPEN_CAP"
  "Property name for per-task open-age cap.")

(defconst org-task-core-repeat-property "ORG_TASK_REPEAT"
  "Property name defining completion behavior: `repeat' or `once'.")

(defconst org-task-core-opened-on-property "ORG_TASK_OPENED_ON"
  "Property name for first-open date of once-tasks.")

(defconst org-task-core-last-done-property "ORG_TASK_LAST_DONE"
  "Property name for last completion date.")

(defconst org-task-core-seed-iterator-property "ORG_TASK_SEED_ITERATOR"
  "Top-level file property name for daily sample reroll iteration.")

(cl-defstruct (org-task-core-task
               (:constructor org-task-core-task-create))
  id marker title weight tweak age-days open-days age-s0 age-alpha age-cap
  open-s0 open-alpha open-cap repeat-mode)

(defun org-task-core-read-number (value fallback)
  "Parse VALUE as a number and return FALLBACK on failure."
  (if (and value (not (string-empty-p value)))
      (let ((parsed (string-to-number value)))
        (if (or (not (numberp parsed))
                (and (zerop parsed) (not (string-match-p "\\`[ \t]*0\\(?:\\.0+\\)?[ \t]*\\'" value))))
            fallback
          (float parsed)))
    fallback))

(defun org-task-core-read-optional-number (value fallback)
  "Parse VALUE as number, allow explicit uncapped markers, else FALLBACK."
  (let ((trimmed (and value (string-trim value))))
    (cond
     ((or (null trimmed) (string-empty-p trimmed))
      fallback)
     ((member (downcase trimmed) '("nil" "none" "uncapped" "infinity" "inf"))
      nil)
     (t
      (org-task-core-read-number trimmed fallback)))))

(defun org-task-core-days-since (date-string)
  "Return days elapsed since DATE-STRING in YYYY-MM-DD format."
  (if (and date-string (not (string-empty-p date-string)))
      (condition-case nil
          (max 0.0
               (/ (float-time (time-subtract (current-time)
                                             (date-to-time (concat date-string " 00:00"))))
                  86400.0))
        (error nil))
    nil))

(defun org-task-core--task-file-buffer (&optional file)
  "Return visiting buffer for FILE or `org-task-file'."
  (find-file-noselect (expand-file-name (or file org-task-file))))

(defun org-task-core--top-scope-limit ()
  "Return position of first heading, or `point-max' when none exists."
  (save-excursion
    (goto-char (point-min))
    (or (when (re-search-forward org-outline-regexp-bol nil t)
          (match-beginning 0))
        (point-max))))

(defun org-task-core-file-property-value (property &optional file)
  "Return top-level PROPERTY value from FILE, or nil when absent."
  (with-current-buffer (org-task-core--task-file-buffer file)
    (save-excursion
      (save-restriction
        (widen)
        (let ((case-fold-search t)
              (limit (org-task-core--top-scope-limit))
              (regexp (format "^[ \t]*#\\+PROPERTY:[ \t]+%s\\(?:[ \t]+\\(.*\\)\\)?[ \t]*$"
                              (regexp-quote property)))
              value)
          (goto-char (point-min))
          (while (re-search-forward regexp limit t)
            (setq value (string-trim (or (match-string-no-properties 1) ""))))
          (unless (string-empty-p (or value ""))
            value))))))

(defun org-task-core-seed-iterator (&optional file)
  "Return top-level seed iterator from FILE, or nil when absent/invalid."
  (let ((value (org-task-core-file-property-value
                org-task-core-seed-iterator-property
                file)))
    (when (and value (string-match-p "\\`[0-9]+\\'" value))
      (string-to-number value))))

(defun org-task-core-set-seed-iterator (value &optional file)
  "Set top-level seed iterator to VALUE in FILE and save it.
VALUE is stored in a `#+PROPERTY:' line and returned as an integer."
  (let* ((iterator (max 0 (prefix-numeric-value value)))
         (line (format "#+PROPERTY: %s %d"
                       org-task-core-seed-iterator-property
                       iterator)))
    (with-current-buffer (org-task-core--task-file-buffer file)
      (save-excursion
        (save-restriction
          (widen)
          (let ((case-fold-search t)
                (limit (org-task-core--top-scope-limit))
                (regexp (format "^[ \t]*#\\+PROPERTY:[ \t]+%s\\(?:[ \t]+\\(.*\\)\\)?[ \t]*$"
                                (regexp-quote org-task-core-seed-iterator-property))))
            (goto-char (point-min))
            (if (re-search-forward regexp limit t)
                (replace-match line t t)
              (goto-char (point-min))
              (while (looking-at-p "^[ \t]*#\\+")
                (forward-line 1))
              (insert line "\n")))))
      (save-buffer))
    iterator))

(defun org-task-core--leaf-heading-p ()
  "Return non-nil if current heading has no children."
  (save-excursion
    (org-back-to-heading t)
    (not (org-goto-first-child))))

(defun org-task-core--open-todo-p ()
  "Return non-nil if current heading has an open TODO state."
  (let ((state (org-get-todo-state)))
    (and state (not (member state org-done-keywords)))))

(defun org-task-core--eligible-heading-p ()
  "Return non-nil when current heading is an eligible sampled task."
  (and (org-task-core--leaf-heading-p)
       (org-task-core--open-todo-p)
       (member org-task-include-tag (org-get-tags nil nil))))

(defun org-task-core-task-id-at-point ()
  "Return task id at point, creating an Org `ID' when missing."
  (or (org-id-get nil nil)
      (org-task-core-ensure-id-at-point)))

(defun org-task-core-ensure-id-at-point ()
  "Ensure current heading has Org `ID' and return it."
  (save-excursion
    (org-back-to-heading t)
    (org-id-get-create)))

(defun org-task-core-normalize-repeat-mode (value)
  "Normalize repeat mode VALUE to symbol `repeat' or `once'."
  (let ((normalized (downcase (string-trim (or value "")))))
    (if (member normalized '("once" "one-shot" "oneshot" "final"))
        'once
      'repeat)))

(defun org-task-core-repeat-mode-at-point ()
  "Return completion behavior at point as `repeat' or `once'."
  (org-task-core-normalize-repeat-mode
   (org-entry-get nil org-task-core-repeat-property t)))

(defun org-task-core--priority-derived-tweak ()
  "Return tweak inferred from Org heading priority at point.
High priority maps to 2.0, low to 0.5, and default/mid to 1.0."
  (let* ((line (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position)))
         (priority (org-get-priority line))
         (high (org-get-priority (format "[#%c]" org-priority-highest)))
         (low (org-get-priority (format "[#%c]" org-priority-lowest))))
    (cond
     ((= priority high) 2.0)
     ((= priority low) 0.5)
     (t 1.0))))

(defun org-task-core--resolve-tweak-at-point ()
  "Resolve tweak at point using explicit property first, then priority mapping."
  (let ((explicit-tweak (org-entry-get nil org-task-core-tweak-property t)))
    (if explicit-tweak
        (org-task-core-read-number explicit-tweak 1.0)
      (org-task-core--priority-derived-tweak))))

(defun org-task-core--make-task ()
  "Build `org-task-core-task' from current heading."
  (let* ((weight (org-task-core-read-number
                  (org-entry-get nil org-task-core-weight-property t)
                  org-task-default-weight))
         (tweak (org-task-core--resolve-tweak-at-point))
         (age-s0 (org-task-core-read-number
                  (org-entry-get nil org-task-core-age-s0-property t)
                  org-task-age-s0))
         (age-alpha (org-task-core-read-number
                     (org-entry-get nil org-task-core-age-alpha-property t)
                     org-task-age-alpha))
         (age-cap (org-task-core-read-optional-number
                   (org-entry-get nil org-task-core-age-cap-property t)
                   org-task-age-cap))
         (open-s0 (org-task-core-read-number
                   (org-entry-get nil org-task-core-open-s0-property t)
                   org-task-open-s0))
         (open-alpha (org-task-core-read-number
                      (org-entry-get nil org-task-core-open-alpha-property t)
                      org-task-open-alpha))
         (open-cap (org-task-core-read-optional-number
                    (org-entry-get nil org-task-core-open-cap-property t)
                    org-task-open-cap))
         (repeat-mode (org-task-core-repeat-mode-at-point))
         (opened-on (org-entry-get nil org-task-core-opened-on-property nil))
         (last-done (org-entry-get nil org-task-core-last-done-property nil))
         (days-since-done (or (org-task-core-days-since last-done) 999.0))
         (days-since-opened (or (org-task-core-days-since opened-on) 0.0)))
    (org-task-core-task-create
     :id (org-task-core-task-id-at-point)
     :marker (point-marker)
     :title (org-get-heading t t t t)
     :weight weight
     :tweak tweak
     :age-days days-since-done
     :open-days days-since-opened
     :age-s0 age-s0
     :age-alpha age-alpha
     :age-cap age-cap
     :open-s0 open-s0
     :open-alpha open-alpha
     :open-cap open-cap
     :repeat-mode repeat-mode)))

(defun org-task-core-collect-tasks (&optional file)
  "Collect eligible tasks from FILE or `org-task-file'."
  (let ((task-file (expand-file-name (or file org-task-file))))
    (when (file-readable-p task-file)
      (with-current-buffer (find-file-noselect task-file)
        (save-excursion
          (save-restriction
            (widen)
            (let (tasks)
              (org-map-entries
               (lambda ()
                 (when (org-task-core--eligible-heading-p)
                   (push (org-task-core--make-task) tasks)))
               nil
               'file)
              (nreverse tasks))))))))

(provide 'org-task-core)

;;; org-task-core.el ends here
