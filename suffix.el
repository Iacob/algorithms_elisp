
(let ()
  (defun make-suffix-table (text)
    (let ((suffix-table (make-vector (length text) -1)))
      (cl-loop for pos from (1- (length text)) downto 1 do
	       (cl-loop for ptn from (1- (length text)) downto 0 do
			(let ((start1 pos) (end1 (1- (length text)))
			      (start2 (- ptn (- (length text) 1 pos))) (end2 ptn)
			      (matched 't)
			      )
			  (if (< start2 0) (setq start2 0))
			  (message "%s %s" start2 end2)
			  (cl-loop for idx1 from end1 downto start1 and idx2 from end2 downto start2 while matched do
				   (if (/= (elt text idx1) (elt text idx2))
				       (setq matched nil)) )
			  (if matched (aset suffix-table pos end2))
			  )
			)
	       )
      suffix-table
      )
    )
  (make-suffix-table "adede")
  )

;; (let ()
;;   (defun make-suffix-table (text)
;;     (let ((suffix-table (make-vector (length text) -1)))
;;       (cl-loop for pos from (1- (length text)) downto 1 do
;; 	       (cl-loop for ptn from (1- pos) downto 0 do
;; 			(let ((start1 pos) (end1 (1- (length text)))
;; 			      (start2 ptn) (end2 (+ ptn (- (1- (length text)) pos)))
;; 			      (matched 't)
;; 			      )
;; 			  ;;(message "%s %s" pos ptn)
;; 			  (cl-loop for idx1 from end1 downto start1 and idx2 from end2 downto start2 while matched do
;; 				   (if (/= (elt text idx1) (elt text idx2))
;; 				       (setq matched nil)) )
;; 			  (if matched (aset suffix-table pos end2))
;; 			  )
;; 			)
;; 	       )
;;       suffix-table
;;       )
;;     )
;;   (make-suffix-table "adede")
;;   )
