;;; org-task.el --- Deterministic task sampling for Org -*- lexical-binding: t; -*-

;; Author: Vardwyn <vardwyn@vardwyn.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.7"))
;; Keywords: outlines, convenience
;;
;; URL: https://github.com/vardwyn/org-task
;;
;; This file is not part of GNU Emacs.
;;
;; Copyright (C) 2026 Vardwyn
;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License version 3
;; as published by the Free Software Foundation
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;
;;; Commentary:
;;
;; org-task provides deterministic weighted sampling for Org tasks.
;; It exposes K/T sampling helpers and agenda integration helpers.

;;; Code:

(require 'org)

(defgroup org-task nil
  "Deterministic daily task selection for Org."
  :group 'org
  :prefix "org-task-")

(defcustom org-task-file (expand-file-name "tasks.org" org-directory)
  "Path to the Org file that stores task headings."
  :type 'file
  :group 'org-task)

(defcustom org-task-include-tag "task"
  "Tag required on eligible leaf task headings."
  :type 'string
  :group 'org-task)

(defcustom org-task-default-weight 1.0
  "Default base weight when `ORG_TASK_WEIGHT' is not set."
  :type 'number
  :group 'org-task)

(defcustom org-task-default-k 5
  "Default number of tasks to sample."
  :type 'integer
  :group 'org-task)

(defcustom org-task-default-temperature 1.0
  "Default sampling temperature. Lower values are greedier."
  :type 'number
  :group 'org-task)

(defcustom org-task-beta-tweak 0.5
  "Exponent for per-task tweak damping."
  :type 'number
  :group 'org-task)

(defcustom org-task-age-s0 7.0
  "Age scale in days for anti-starvation boosting."
  :type 'number
  :group 'org-task)

(defcustom org-task-age-alpha 1.2
  "Exponent for anti-starvation age boosting."
  :type 'number
  :group 'org-task)

(defcustom org-task-age-cap nil
  "Optional cap for age boost. Nil means uncapped."
  :type '(choice (const :tag "No cap" nil) number)
  :group 'org-task)

(defcustom org-task-open-pressure-steps '((0 . 0.0) (3 . 0.6) (5 . 1.0) (10 . 1.8))
  "Open-pressure levels as (OPEN-COUNT . LEVEL)."
  :type '(repeat (cons integer number))
  :group 'org-task)

(defcustom org-task-open-s0 7.0
  "Open-age scale in days for open-pressure boost."
  :type 'number
  :group 'org-task)

(defcustom org-task-open-alpha 1.2
  "Exponent for open-pressure boosting."
  :type 'number
  :group 'org-task)

(defcustom org-task-open-cap nil
  "Optional cap for open-pressure boost. Nil means uncapped."
  :type '(choice (const :tag "No cap" nil) number)
  :group 'org-task)

(defcustom org-task-once-backlog-keyword "TODO"
  "Keyword representing once-task backlog state before first opening."
  :type 'string
  :group 'org-task)

(defcustom org-task-once-active-keyword "STRT"
  "Keyword representing once-task active/open state."
  :type 'string
  :group 'org-task)

(defcustom org-task-finalize-keyword "FIN"
  "Done keyword used for explicit finalization of tasks."
  :type 'string
  :group 'org-task)

(defvar org-state nil
  "Dynamic TODO state variable from Org hooks.")

(require 'org-task-core)
(require 'org-task-sampling)
(require 'org-task-runtime)

;;;###autoload
(defun org-task-sample-tasks (&optional k temperature seed)
  "Return sampled task structs for K picks at TEMPERATURE using SEED.
Defaults are taken from `org-task-default-k', `org-task-default-temperature',
and today's local date seed."
  (interactive)
  (org-task-runtime-sample-tasks k temperature seed))

;;;###autoload
(defun org-task-reroll-today ()
  "Bump today's file-level seed iterator and return the new effective seed."
  (interactive)
  (let ((seed (org-task-runtime-reroll-today)))
    (when-let* ((agenda-buffer (and (boundp 'org-agenda-buffer-name)
                                    (get-buffer org-agenda-buffer-name))))
      (with-current-buffer agenda-buffer
        (when (and (derived-mode-p 'org-agenda-mode)
                   (fboundp 'org-agenda-redo))
          (org-agenda-redo))))
    (message "org-task: rerolled today's sample (%s)" seed)
    seed))

;;;###autoload
(defun org-task-agenda-skip-non-sampled (&optional k temperature seed)
  "Agenda skip function keeping only sampled tasks for K/T/SEED."
  (org-task-runtime-agenda-skip-non-sampled k temperature seed))

;;;###autoload
(defun org-task-handle-done ()
  "Hook handler that applies org-task done transitions in Org/Agenda contexts."
  (when (and org-state
             (member org-state org-done-keywords))
    (org-task-runtime-handle-done org-state)))

;;;###autoload
(define-minor-mode org-task-mode
  "Global mode for org-task done-state handling."
  :global t
  :group 'org-task
  (if org-task-mode
      (add-hook 'org-after-todo-state-change-hook #'org-task-handle-done)
    (remove-hook 'org-after-todo-state-change-hook #'org-task-handle-done)))

;;;###autoload
(defun org-task-agenda-block (&optional header k temperature seed)
  "Return reusable sampled-task `org-agenda-custom-commands' block.
HEADER defaults to \"Org Task Sample\".
K, TEMPERATURE, and SEED are passed to `org-task-agenda-skip-non-sampled'."
  `(tags ,org-task-include-tag
         ((org-agenda-overriding-header ,(or header "Org Task Sample"))
          (org-agenda-files (list org-task-file))
          (org-agenda-skip-function
           (lambda ()
             (org-task-agenda-skip-non-sampled ,k ,temperature ,seed))))))

(provide 'org-task)

;;; org-task.el ends here
