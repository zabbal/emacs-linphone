;;; -*- show-trailing-whitespace: t -*-
;;; linphone-modeline.el --- emacs-linphone mode-line display

;; Copyright (C) 2010 Yoni Rabkin
;;
;; Author: Yoni Rabkin <yonirabkin@member.fsf.org>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Code:

(require 'timer)
(require 'linphone)

(defvar linph-mode-line-mode-active nil
  "If non-nil display status in mode-line.")

(defvar linph-mode-line-string ""
  "String to display in the mode-line.")

(defvar linph-mode-line-update-timer nil
  "Interval timer object.")

(defvar linph-mode-line-update-timer-interval 30
  "timer object interval.")

(defvar linph-mode-line-in-call-string "c"
  "String to display in mode line when in-call.")

(defvar linph-mode-line-on-hook-string "h"
  "String to display in mode line when on hook.")

(defvar linph-mode-line-incoming-call-string "i"
  "String to display in mode line during an incoming call.")

(defun linph-update-mode-line-status (str)
  "Update the mode line with STR."
  (setq linph-mode-line-string
	(concat " [" str "] "))
  (force-mode-line-update))

(defun linph-poll-status ()
  "Return a string associated with the process status."
  (let ((status (linph-command-alive-p)))
    (if status
	(cond ((equal status 'in-call)
	       linph-mode-line-in-call-string)
	      ((equal status 'incoming-call)
	       linph-mode-line-incoming-call-string)
	      (t linph-mode-line-on-hook-string))
      (linph-mode-line-deactivate))))

(defun linph-mode-line-update ()
  "Read the process status and update the mode line."
  (linph-update-mode-line-status (linph-poll-status)))

;; annoying
(defun linph-retarded-mode-line-update ()
  "Wait for the process to update its status before querying."
  (run-at-time "1 seconds" nil 'linph-mode-line-update))

(defun linph-mode-line-deactivate ()
  "Deactivate mode line display."
  (when (and linph-mode-line-update-timer
	     (timerp linph-mode-line-update-timer))
    (cancel-timer linph-mode-line-update-timer))
  (remove-hook 'linph-state-change-hook 'linph-mode-line-update)
  (setq global-mode-string
	(delq 'linph-mode-line-string global-mode-string))
  (setq linph-mode-line-mode-active nil))

(defun linph-mode-line-activate ()
  "Activate mode line display."
  (linph-mode-line-deactivate)
  (add-to-list 'global-mode-string 'linph-mode-line-string t)
  (add-hook 'linph-state-change-hook 'linph-retarded-mode-line-update)
  (setq linph-mode-line-update-timer
	(run-at-time nil linph-mode-line-update-timer-interval
		     'linph-mode-line-update))
  (setq linph-mode-line-mode-active t))

(defun linph-mode-line (arg)
  "Turn on `linph-mode-line' if ARG positive, otherwise off."
  (interactive "p")
  (or global-mode-string (setq global-mode-string '("")))
  (if (and arg
	   (numberp arg)
	   (> arg 0))
      (linph-mode-line-activate)
    (linph-mode-line-deactivate)))

(provide 'linphone-modeline)

;;; linphone.el ends here.
