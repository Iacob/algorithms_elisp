
(defun binary-search (lst val)
  (let ((search-range (vector 0 (1- (seq-length lst))))
	(idx-found nil)
	(stop-search nil))
    (while (and (< (seq-elt search-range 0) (seq-elt search-range 1))
		(not stop-search))
      (let* ((start-pos (seq-elt search-range 0))
	     (end-pos (seq-elt search-range 1))
	     (mid-pos (+ (/ (- end-pos start-pos) 2) start-pos))
	     (mid-val (seq-elt lst mid-pos)))
	
	(cond ((= mid-val val)
	       (setq idx-found mid-pos)
	       (setq stop-search 't))
	      ((> mid-val val)
	       (aset search-range 1 (1- mid-pos)))
	      ((< mid-val val)
	       (aset search-range 0 (1+ mid-pos))) ) ) )
    idx-found)
  )

(binary-search [4 5 6 7] 5)
