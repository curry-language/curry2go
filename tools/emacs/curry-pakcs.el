;; curry-pakcs.el --- simplistic interaction mode with a
;; PAKCS interpreter for Curry developped by 
;;    The University of Nottingham and Yale University, 1994-1997.
;;    Web: http://www.curry.org/pakcs.
;; In standard Emacs terminology, this would be called
;;    inferior-pakcs-mode
;;    
;; Copyright 1999 Guy Lapalme

;; Author:1998, 1999 Guy Lapalme <lapalme@iro.umontreal.ca>

;; Keywords: PAKCS inferior mode, PAKCS interaction mode
;; Version: 1.1
;; URL: http://www.iro.umontreal.ca/~lapalme/Hugs-interaction.html

;;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;; Commentary:

;; Purpose:
;;
;; To send a Curry buffer to another buffer running a PAKCS interpreter
;; The functions are adapted from  the Hugs Mode developed by
;;         Chris Van Humbeeck <chris.vanhumbeeck@cs.kuleuven.ac.be>
;;
;; Installation:
;; 
;; To use with the Curry mode of 
;;        Moss&Thorn <http://www.curry.org/curry-mode>
;; add this to .emacs:
;;
;;    (add-hook curry-mode-hook 'turn-on-curry-pakcs)
;;
;; Customisation:
;;       The name of the pakcs interpreter is in variable
;;          curry-pakcs-program-name
;;       Arguments can be sent to the PAKCS interpreter when it is called
;;       by setting the value of the variable
;;          curry-pakcs-program-args
;;
;;       This value can be interactively by calling C-cC-s with an
;;       argument. 
;;
;;       If the command does not seem to respond, see the
;;          content of the `comint-prompt-regexp' variable
;;          to check that it waits for the appropriate PAKCS prompt
;;          the current value is appropriate for PAKCS 1.6
;;
;;
;;    `curry-pakcs-hook' is invoked in the *pakcs* once it is started.
;;    
;;; All functions/variables start with
;;; `(turn-(on/off)-)curry-pakcs' or `curry-pakcs-'.

(defun turn-on-curry-pakcs ()
  "Turn on Curry interaction mode with a PAKCS interpreter running in an
another Emacs buffer named *pakcs*.
Maps the followind commands in the curry keymap.
     \\[curry-pakcs-load-file] to save the current buffer and load it by sending the :load command to PAKCS.
     \\[curry-pakcs-reload-file] to send the :reload command to PAKCS without saving the buffer.
     \\[curry-pakcs-show-pakcs-buffer] to show the PAKCS buffer and go to it."
  (interactive)
  (local-set-key "\C-c\C-s" 'curry-pakcs-start-process)
  (local-set-key "\C-c\C-l" 'curry-pakcs-load-file)
  (local-set-key "\C-c\C-r" 'curry-pakcs-reload-file)
  (local-set-key "\C-c\C-b" 'curry-pakcs-show-pakcs-buffer)
  )

(defun turn-off-curry-pakcs ()
  "Turn off Curry interaction mode with a PAKCS interpreter within a buffer."
  (interactive)
  (local-unset-key  "\C-c\C-s")
  (local-unset-key  "\C-c\C-l")
  (local-unset-key  "\C-c\C-r")
  (local-unset-key  "\C-c\C-b")
  )

(defvar curry-pakcs-mode-map nil)

(defun curry-pakcs-mode ()
;; called by curry-pakcs-start-process,
;; itself called by curry-pakcs-load-file
;; only when the file is loaded the first time
  "Major mode for interacting with an inferior PAKCS session.

The commands available from within a Curry script are:
     \\<curry-mode-map>\\[curry-pakcs-load-file] to save the current buffer and load it by sending the :load command to PAKCS.
     \\[curry-pakcs-reload-file] to send the :reload command to PAKCS without saving the buffer.
     \\[curry-pakcs-show-pakcs-buffer] to show the PAKCS buffer and go to it.

\\<curry-pakcs-mode-map>
Commands:
Return at end of buffer sends line as input.
Return not at end copies rest of line to end and sends it.
\\[comint-kill-input] and \\[backward-kill-word] are kill commands, imitating normal Unix input editing.
\\[comint-interrupt-subjob] interrupts the comint or its current subjob if any.
\\[comint-stop-subjob] stops, likewise. \\[comint-quit-subjob] sends quit signal."
  (interactive)
  (comint-mode)
  (setq major-mode 'curry-pakcs-mode)
  (setq mode-name "Curry PAKCS")
  (if curry-pakcs-mode-map
      nil
    (setq curry-pakcs-mode-map (copy-keymap comint-mode-map))
    )
  (use-local-map curry-pakcs-mode-map)
  )

;; PAKCS-interface

(require 'comint)
(require 'shell)

(defvar curry-pakcs-process nil
  "The active PAKCS subprocess corresponding to current buffer.")

(defvar curry-pakcs-process-buffer nil
  "Buffer used for communication with PAKCS subprocess for current buffer.")

(defvar curry-pakcs-last-loaded-file nil
  "The last file loaded into the PAKCS process.")

(defvar curry-pakcs-program-name "curry2go"
  "The name of the command to start Curry2Go.")

(defvar curry-pakcs-program-args '("--noreadline")
  "A list of string args to send to the pakcs process")

(defvar curry-pakcs-load-end nil
  "Position of the end of the last load command")

(defvar curry-pakcs-send-end nil
  "Position of the end of the last send command")

(defun curry-pakcs-start-process (arg)
  "Start a PAKCS process and invokes `curry-pakcs-hook' if not nil.
Prompts for a list of args if called with an argument."
  (interactive "P")
  (message "Starting `pakcs-process' %s" curry-pakcs-program-name)
  (if arg
      (setq curry-pakcs-program-args
            (read-minibuffer "List of args for PAKCS:"
                             (prin1-to-string curry-pakcs-program-args))))
  (setq curry-pakcs-process-buffer
        (apply 'make-comint
               "curry2go" curry-pakcs-program-name nil
               curry-pakcs-program-args))
  (setq curry-pakcs-process
        (get-buffer-process curry-pakcs-process-buffer))
  ;; Select PAKCS buffer temporarily
  (set-buffer curry-pakcs-process-buffer)
  (curry-pakcs-mode)
  (make-variable-buffer-local 'shell-cd-regexp)
  (make-local-variable 'shell-dirtrackp)
  (setq shell-cd-regexp         ":cd")
  (setq shell-dirtrackp         t)
  (setq comint-input-sentinel   'shell-directory-tracker)
                                ; ? or  module name in hugs
  ;;(setq comint-prompt-regexp  "^\? \\|^[A-Z][_a-zA-Z0-9]*> ")
      ; module name > or result ? in PAKCS
  (setq comint-prompt-regexp  "^[/a-zA-Z].*> \\|^.*\? ")
    ;; comint's history syntax conflicts with PAKCS syntax, eg. !!
  (setq comint-input-autoexpand nil)
  (run-hooks 'curry-pakcs-hook)
  (message "")
  )

(defun curry-pakcs-wait-for-output ()
  "Wait until output arrives and go to the last input."
  (while (progn			
	   (goto-char comint-last-input-end)
	   (not (re-search-forward comint-prompt-regexp nil t)))
    (accept-process-output curry-pakcs-process)))

(defun curry-pakcs-send (&rest string)
  "Send curry-pakcs-process the arguments (one or more strings).
A newline is sent after the strings and they are inserted into the
current buffer after the last output."
  ;; Wait until output arrives and go to the last input.
  (curry-pakcs-wait-for-output)
  ;; Position for this input.
  (goto-char (point-max))		
  (apply 'insert string)
  (comint-send-input)
  (setq curry-pakcs-send-end (marker-position comint-last-input-end)))

(defun curry-pakcs-go (load-command cd)
  "Save the current buffer and load its file into the PAKCS process.
The first argument LOAD-COMMAND specifies how the file should be
loaded: as a new file (\":load \") or as a reload (\":reload \").

If the second argument CD is non-nil, change the Curry-PAKCS process to the
current buffer's directory before loading the file.

If the variable \"curry-pakcs-command\" is set then its value will be sent to
the PAKCS process after the load command.  This can be used for a
top-level expression to evaluate."
  (let (file)
    (hack-local-variables);; In case they've changed
    (untabify (point-min) (point-max)) ;; replace all tabs by spaces
    (save-buffer)
    (if (string-equal load-command ":load ")
        (progn
          (setq file (buffer-file-name))
          (setq curry-pakcs-last-loaded-file file))
      (setq file ""))
    (let ((dir (expand-file-name default-directory))
          (cmd (and (boundp 'curry-pakcs-command)
                    curry-pakcs-command
                    (if (stringp curry-pakcs-command)
                        curry-pakcs-command
                      (symbol-name curry-pakcs-command)))))
      (if (and curry-pakcs-process-buffer
               (eq (process-status curry-pakcs-process) 'run))
	  ;; Ensure the PAKCS buffer is selected.
	  (set-buffer curry-pakcs-process-buffer)
        ;; Start Curry-PAKCS process.
        (curry-pakcs-start-process nil))
 
      (if cd (curry-pakcs-send (concat ":cd " dir)))
      ;; Wait until output arrives and go to the last input.
      ;(curry-pakcs-wait-for-output)
      (curry-pakcs-send load-command file)
      ;; Error message search starts from last load command.
      (setq curry-pakcs-load-end (marker-position comint-last-input-end))
      (if cmd (curry-pakcs-send cmd))
      ;; Wait until output arrives and go to the last input.
      (curry-pakcs-wait-for-output)))
  )

(defun curry-pakcs-load-file (cd)
  "Save a pakcs buffer file and load its file.
If CD (prefix argument if interactive) is non-nil, change the PAKCS
process to the current buffer's directory before loading the file.
If there is an error, set the cursor at the error line otherwise show
the PAKCS buffer."
  (interactive "P")
  (curry-pakcs-gen-load-file ":load " cd)
  )
 
(defun curry-pakcs-reload-file (cd)
  "Save a pakcs buffer file and load its file.
If CD (prefix argument if interactive) is non-nil, change the PAKCS
process to the current buffer's directory before loading the file.
If there is an error, set the cursor at the error line otherwise show
the PAKCS buffer."
  (interactive "P")
  (curry-pakcs-gen-load-file ":reload " cd)
  )

(defun curry-pakcs-gen-load-file (cmd cd)
  "Save a pakcs buffer file and load its file or reload depending on CMD.
If CD is non-nil, change the process to the current buffer's directory 
before loading the file. If there is an error, set the cursor at the 
error line otherwise show the PAKCS buffer."
  (save-excursion (curry-pakcs-go cmd cd))
  ;; Ensure the PAKCS buffer is selected.
  (set-buffer curry-pakcs-process-buffer)
  ;; Error message search starts from last load command.
  (goto-char curry-pakcs-load-end)
  (if (re-search-forward
       ;;"^ERROR \"\\([^ ]*\\)\"\\( (line \\([0-9]*\\))\\|\\)" nil t)
       "^ERROR: " nil t)
      (let ((efile (buffer-substring (match-beginning 1)
				     (match-end 1)))
	    (eline (if (match-beginning 3)
                       (string-to-int (buffer-substring (match-beginning 3)
                                                        (match-end 3)))))
	    (emesg (buffer-substring (1+ (point))
				     (save-excursion (end-of-line) (point)))))
        (pop-to-buffer  curry-pakcs-process-buffer) ; show *pakcs* buffer
        (goto-char (point-max))
        (recenter)
	;;(message "PAKCS error %s %s" (file-name-nondirectory efile) emesg)
	(message "Error during compilation")
        (if (file-exists-p efile)
            (progn (find-file-other-window efile)
                   (if eline (goto-line eline))
                   (recenter)))
        )
    (pop-to-buffer  curry-pakcs-process-buffer) ; show *pakcs* buffer
    (goto-char (point-max))
    ;;(message "There were no errors.")
    (recenter 3)                        ; show only the end...
    )
  )

(defun curry-pakcs-show-pakcs-buffer ()
  "Goes to the PAKCS buffer."
  (interactive)
  (if (or (not curry-pakcs-process-buffer)
          (not (buffer-live-p curry-pakcs-process-buffer)))
      (curry-pakcs-start-process nil))
  (pop-to-buffer  curry-pakcs-process-buffer)
  )
