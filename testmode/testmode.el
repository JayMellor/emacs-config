;;;; testmode.el --- definitely a useful file

(defvar testmode-mode-map
  (let ((map (make-sparse-keymap)))
	(prog1 map
	  (suppress-keymap map)
	  (define-key map "f" #'testmode-insert-fred)
	  (define-key map "g" #'testmode-message)
	  (define-key map "x" (lambda ()
							(interactive)
							(let ((inhibit-read-only t))
							  (insert "help\n")))))))
(defun testmode ()
  "Opens a buffer called testbuff with fred in"
  (interactive)
  (switch-to-buffer (get-buffer-create "testbuff"))
  (unless (eq major-mode 'testmode-mode)
    (testmode-mode)))

(defun testmode-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map testmode-mode-map)
  (setf major-mode 'testmode-mode
		mode-name "my test mode"
		buffer-read-only t
		header-line-format "test")
  (testmode-insert-fred))

(defun testmode-insert-fred ()
  (interactive)
  (message "called")
  (let ((inhibit-read-only t))	
	(insert "fred")))

(defun testmode-message ()
  (interactive)
  (message "Hey"))

(provide 'testmode)
