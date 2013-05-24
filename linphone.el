;;; -*- show-trailing-whitespace: t -*-
;;; linphone.el --- Emacs interface to Linphone

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

;;; Commentary:
;;
;; Written against Linphone version 3.3.2 on GNU/Linux

;;; Code:

(defvar linphcsh-binary "/usr/local/bin/linphonecsh"
  "Path of the linphonecsh binary.")

(defvar linph-configuration "~/.linphonerc"
  "Path of the linphone configuration file.")

(defvar linph-contacts-hashtable nil
  "A hash-table of lists representing a phonebook. The key is the
contact NAME. The value is the list. Each list is of the
form: (name phone type). NAME and PHONE are strings. TYPE is a
symbol.")

(defvar linph-providers nil
  "A list of lists representing VOIP providers. These should
correspond to those in the ~/.linphonerc file. Each sublist is of
the form: (name proxy-name type-list). NAME and
PROXY-NAME are strings. TYPE-LIST is a list of symbols.")

;; Trying to keel all of the scanner strings and regular expressions
;; up here and out of the code. No idea if the next version of
;; linphonec/sh will change them and it will be a drag to have to comb
;; through the code to find them all.

(defvar linph-pair-regexp "^\\(.*\\)=\\(.*\\)$"
  "Scanner string for a name-value pair.")

(defvar linph-already-running-string
  (concat "A running linphonec has been"
	  " found, not spawning a second one."
	  "\n")
  "Scanner string for running instance.")

(defvar linph-hook-string "hook="
  "Scanner string for hook status.")

(defvar linph-in-call-string "Call "
  "Scanner string for in-call status.")

(defvar linph-incoming-call-string "Incom"
  "Scanner string for incoming-call status.")

(defvar linph-not-running-string
  (concat "ERROR: Failed to connect"
	  " pipe: Connection refused\n")
  "Scanner string for when linphone isn't running.")

(defvar linph-start-hook nil
  "Hook called after linphone starts.")

(defvar linph-quit-hook nil
  "Hook called after linphone quits.")

(defvar linph-call-hook nil
  "Hook called after a call is made.")

(defvar linph-disconnect-hook nil
  "Hook called after a call is ended.")

(defvar linph-answer-hook nil
  "Hook called after a call is answered.")

(defvar linph-state-change-hook nil
  "Hook called when there is likely a linphone state change.")

(defun linph-wait (message sec)
  "Display MESSAGE with rolling ellipsis while sleeping SEC."
  (if (and (not (stringp message))
	   (not (integerp sec)))
      (error "bad arguments: %s %s" message sec)
    (let ((ellipsis "."))
      (dotimes (c sec)
	(message "%s%s" message ellipsis)
	(sleep-for 1)
	(setq ellipsis (concat ellipsis "."))))))

(defun linph-command (&rest args)
  "Send the command ARGS to the linphone process.
Returns whatever the linphone process returned as a string."
  (with-temp-buffer
    (condition-case nil
	(apply 'call-process (append
			      `(,linphcsh-binary)
			      `(nil t nil)
			      args))
      (file-error (error "linphonecsh binary not found")))
    (buffer-substring-no-properties
     (point-min) (point-max))))

(defun linph-parse-name-value-pair (str)
  "Parse the name-value pair in STR or barf."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (if (re-search-forward linph-pair-regexp (point-max) t)
	(let ((command (match-string-no-properties 1))
	      (status  (match-string-no-properties 2)))
	  (if (and command status)
	      (cons command status)
	    (error "could not parse: %s" str)))
      (error "could not parse: %s" str))))

(defun linph-command-alive-p ()
  "Return a truth value if linphone is running."
  (let ((output (linph-command "status" "hook")))
    (cond ((string= (substring output 0 5)
		    linph-hook-string)
	   t)
	  ((string= (substring output 0 5)
		    linph-in-call-string)
	   'in-call)
	  ((string= (substring output 0 5)
		    linph-incoming-call-string)
	   'incoming-call)
	  ((string= output
		    linph-not-running-string) nil)
	  (t (error "unhandled response: %s" output)))))

(defun linph-assert-alive ()
  "Throw an error if linphonec isn't running."
  (let ((state (linph-command-alive-p)))
    (if (not state)
	(error "linphonec isn't running")
      state)))

(defun linph-send-status-command (&rest commands)
  "Send COMMANDS and parse the result as a name-value pair."
  (linph-assert-alive)
  (linph-parse-name-value-pair
   (apply 'linph-command commands)))

(defun linph-send-command (&rest commands)
  "Send COMMANDS."
  (linph-assert-alive)
  (apply 'linph-command commands))

(defun linph-send-call (identity registrar)
  "Place a call to IDENTITY via REGISTRAR."
  (let ((state (linph-assert-alive)))
    (cond ((equal state 'in-call)
	   (error "terminate current call first"))
	  (state
	   (linph-send-command
	    "generic" (concat "call sip:" identity "@" registrar))
	   (run-hooks 'linph-call-hook)
	   (run-hooks 'linph-state-change-hook))
	  (t (error "unhandled call error")))))

(defun linph-command-init (&optional no-error)
  "Start a background instance of linphonec."
  (when (string= (linph-command "init" "-c"
				(expand-file-name
				 linph-configuration))
		 linph-already-running-string)
    (if no-error
	(message "linphonec already running.")
      (error "linphonec already running.")))
  (linph-wait "waiting for linphone to start" 2)
  (if (linph-command-alive-p)
      (progn
	(message "linphonec successfully started")
	(run-hooks 'linph-quit-hook))
    (error "could not start linphone")))

(defun linph-command-exit ()
  "Kill the running linphone."
  (linph-command "exit")
  (linph-wait "waiting for linphone to shut down" 2)
  (if (not (linph-command-alive-p))
      (message "linphonec successfully shut down")
    (error "failed to shut down linphonec")))

(defun linph-search-providers (contact)
  "Return the provider suitable for calling CONTACT."
  (let ((type (nth 2 contact))
	(providers (copy-tree linph-providers))
	result)
    ;; the list of providers will always be short so iterating through
    ;; the list each time anew is effectively constant time
    (while providers
      (let ((current (car providers)))
	(if (member type (nth 2 current))
	    (progn
	      (setq result current)
	      (setq providers nil))
	  (setq providers (cdr providers)))))
    result))

(defun linph-call-contact (contact)
  "Place a call to CONTACT using suitable provider."
  (let ((provider (linph-search-providers contact)))
    (when (not provider)
      (error "no provider for contact: %s" (car contact)))
    (let ((registrar (cadr provider))
	  (identity  (cadr contact))
	  (name      (car  contact)))
      (linph-send-call identity registrar)
      (message "calling %s via %s" name registrar)
      (run-hooks 'linph-call-hook)
      (run-hooks 'linph-state-change-hook))))

(defun linph-call (contact)
  "Interactively place a call to CONTACT."
  (interactive
   (list
    (completing-read "Contact: "
		     (let (e)
		       (maphash
			'(lambda (k v)
			   (setq e (append (list k) e)))
			linph-contacts-hashtable)
		       e))))
  (linph-call-contact (gethash contact linph-contacts-hashtable)))

(defun linph-answer ()
  "Answer the call."
  (interactive)
  (linph-send-command "generic" "answer")
  (run-hooks 'linph-answer-hook)
  (run-hooks 'linph-state-change-hook))

(defun linph-terminate ()
  "Terminate the call."
  (interactive)
  (linph-send-command "generic" "terminate")
  (run-hooks 'linph-disconnect-hook)
  (run-hooks 'linph-state-change-hook))

(defun linph-quit ()
  "Start linphone."
  (interactive)
  (linph-command-exit))

(defun linph-start (&optional no-error)
  "Quit linphone."
  (interactive)
  (linph-command-init no-error))

(provide 'linphone)

;;; linphone.el ends here.
