;;; org-task-sampling.el --- Weighted deterministic sampling -*- lexical-binding: t; -*-
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

;; Computes sampling weights and performs deterministic weighted top-k selection.

;;; Code:

(require 'cl-lib)
(require 'org-task-core)

(defvar org-task-default-weight)
(defvar org-task-beta-tweak)
(defvar org-task-age-s0)
(defvar org-task-age-alpha)
(defvar org-task-age-cap)
(defvar org-task-open-pressure-steps)
(defvar org-task-open-s0)
(defvar org-task-open-alpha)
(defvar org-task-open-cap)

(defconst org-task-sampling--uniform-base (float (expt 16 12))
  "Denominator used for deterministic pseudo-random values in (0,1).")

(defun org-task-sampling-date-seed (&optional time)
  "Return YYYY-MM-DD seed string for TIME in system local time."
  (format-time-string "%Y-%m-%d" (or time (current-time))))

(defun org-task-sampling--age-boost (days age-s0 age-alpha age-cap)
  "Compute anti-starvation boost from DAYS with AGE-S0/AGE-ALPHA/AGE-CAP."
  (let* ((value (+ 1.0
                   (expt (/ (max 0.0 (or days 0.0))
                            (max 0.001 (float age-s0)))
                         (max 0.0 (float age-alpha))))))
    (if (numberp age-cap)
        (min value (float age-cap))
      value)))

(defun org-task-sampling--open-pressure-level (n-open)
  "Resolve open-pressure level for N-OPEN from configured thresholds."
  (let ((level 0.0))
    (dolist (step (sort (copy-sequence org-task-open-pressure-steps)
                        (lambda (left right) (< (car left) (car right)))))
      (when (>= n-open (car step))
        (setq level (cdr step))))
    (float level)))

(defun org-task-sampling--open-boost (days n-open open-s0 open-alpha open-cap)
  "Compute open-task pressure boost from DAYS and N-OPEN."
  (let* ((pressure (org-task-sampling--open-pressure-level n-open))
         (value (+ 1.0
                   (* pressure
                      (expt (/ (max 0.0 (or days 0.0))
                               (max 0.001 (float open-s0)))
                            (max 0.0 (float open-alpha)))))))
    (if (numberp open-cap)
        (min value (float open-cap))
      value)))

(defun org-task-sampling-raw-weight (task n-open)
  "Return raw weight for TASK given current open task count N-OPEN."
  (let* ((base (max 0.001
                    (float (or (org-task-core-task-weight task)
                               org-task-default-weight
                               1.0))))
         (tweak (max 0.001 (float (or (org-task-core-task-tweak task) 1.0))))
         (beta (max 0.0 (float org-task-beta-tweak))))
    (max 1e-9
         (* base
            (expt tweak beta)
            (org-task-sampling--age-boost
             (org-task-core-task-age-days task)
             (or (org-task-core-task-age-s0 task) org-task-age-s0 7.0)
             (or (org-task-core-task-age-alpha task) org-task-age-alpha 1.0)
             (org-task-core-task-age-cap task))
            (org-task-sampling--open-boost
             (org-task-core-task-open-days task)
             n-open
             (or (org-task-core-task-open-s0 task) org-task-open-s0 7.0)
             (or (org-task-core-task-open-alpha task) org-task-open-alpha 1.0)
             (org-task-core-task-open-cap task))))))

(defun org-task-sampling-effective-weight (task n-open temperature)
  "Return temperature-adjusted weight for TASK."
  (let ((temp (max 0.001 (float (or temperature 1.0)))))
    (max 1e-9
         (expt (org-task-sampling-raw-weight task n-open)
               (/ 1.0 temp)))))

(defun org-task-sampling--uniform (seed id)
  "Return deterministic pseudo-random number in (0,1) for SEED and ID."
  (let* ((hex (substring (secure-hash 'sha1 (format "%s:%s" seed id)) 0 12))
         (value (string-to-number hex 16)))
    (/ (+ value 1.0) (+ org-task-sampling--uniform-base 1.0))))

(defun org-task-sampling--selection-key (task seed temperature n-open)
  "Compute deterministic weighted top-k key for TASK."
  (let* ((weight (org-task-sampling-effective-weight task n-open temperature))
         (u (org-task-sampling--uniform seed (org-task-core-task-id task))))
    (/ (- (log u)) weight)))

(defun org-task-sampling-select (tasks k temperature seed &optional n-open)
  "Select up to K tasks from TASKS with deterministic weighted sampling.
TEMPERATURE controls exploration and SEED fixes deterministic randomness.
N-OPEN defaults to `(length TASKS)`."
  (let* ((open-count (or n-open (length tasks)))
         (limit (min (max 0 (or k 0)) (length tasks)))
         (ranked (cl-loop for task in tasks
                          for key = (org-task-sampling--selection-key
                                     task seed temperature open-count)
                          collect (cons key task))))
    (setq ranked (cl-sort ranked #'< :key #'car))
    (cl-loop for (_ . task) in ranked
             for index from 1
             while (<= index limit)
             collect task)))

(provide 'org-task-sampling)

;;; org-task-sampling.el ends here
