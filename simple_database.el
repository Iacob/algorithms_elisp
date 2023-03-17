
(let ()

  ;;(setq **dbe-current-id** 1)
  (defun dbe-next-id ()
    (unless (boundp '**dbe-current-id**) (setq **dbe-current-id** 1))
    (let ((id **dbe-current-id**))
      (cl-incf **dbe-current-id**)
      id ) )

  (defun dbe-rows (fn-handle-row &rest params)
    (let ((has-more 't) current-line linum1 linum2
	  (fn-continue (plist-get params :continue)))
      (save-window-excursion
	(switch-to-buffer "**content**")
	(beginning-of-buffer)
	(if fn-continue (setq has-more (funcall fn-continue)))
	(while has-more
	  (setq current-line
		(buffer-substring (line-beginning-position)
				  (line-end-position)))
	  (unless (string= current-line "")
	    (funcall fn-handle-row (read current-line)))
	  (if fn-continue (setq has-more (funcall fn-continue)))
	  (setq linum1 (line-number-at-pos (point)))
	  (forward-line)
	  (setq linum2 (line-number-at-pos (point)))
	  (if (= linum1 linum2) (setq has-more nil))
	  )
	)
      )
    )
  
  (defun dbe-insert (row)
    (interactive "xPlease enter the row data in plist format: ")
    (save-window-excursion
      (switch-to-buffer "**content**")
      (end-of-buffer)
      (let (row-data)
	(setq row-data (append (list 'id (dbe-next-id)) (ensure-list row)))
	(insert (format "%s\n" row-data))
	(message ">> Row added: %s" row-data) ) ) )

  (defun dbe-find-by-id (row-id)
    (interactive "nPlease enter row id: ")
    (let (row-found)
      (dbe-rows (lambda (row)
		  ;;(message "  checking row: %s" row)
		  (when (equal (plist-get row 'id) row-id)
		    (setq row-found row)) )
		:continue (lambda () (null row-found)))
      (message ">> Row found: %s" row-found)
      )
    
    )
  

  (dbe-rows (lambda (row)
	      (message "--%s" row) )
	    :continue (lambda () 't))
  )
