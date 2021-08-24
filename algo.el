
;; (defun -charCodeAt (str pos)
;;   (if (< pos (length str))
;;       (let ((ch1 (elt str pos)))
;; 	(- ch1 97)
;; 	)
;;     -1)
;;   )

(defun -charCodeAt (str pos)
  (if (< pos (length str))
      (elt str pos)
    -1)
  )

(defun cfor (&rest params)
  (let ((init-val (plist-get params :init))
	(conds (plist-get params :when))
	(next-val-func (plist-get params :next))
	(body (plist-get params :body)))
    
    (let ((curr-val init-val))
      (while (funcall conds curr-val)
	(when body
	  (funcall body curr-val)
	  )
	(setq curr-val (funcall next-val-func curr-val))
	)
      )
    )
  )

;; (let ()
;;   (print "================")
;;   (cfor :init 0 :when (lambda(x) (< x 5)) :next '1+ :body (lambda(x) (print "-")))
;;   (print "================")
;;   )

(defun bm_compile_pattern (pattern)
  (let* ((R 256) (M (length pattern)) (right (make-vector R -1)))
    (let ((j -1))
      (while (progn (setq j (1+ j)) (< j M))
	(aset right (-charCodeAt pattern j) j) ) )
    right
    )
  )

;;(bm_compile_pattern "abcdb")


(defun bm_substring_search (pattern text)
  "Boyer-Moore substring search"
  (let ((txtLen (length text)) (patLen (length pattern))
	(right nil) (skip 0) (result nil))
    
    (setq right (bm_compile_pattern pattern))
    (let ((startPos nil) (patPos nil))
      (while (progn (setq startPos (if (not startPos) 0 (+ startPos skip)) )
       		    (and (not result) (<= startPos (- txtLen patLen))) )
	(setq skip 0)
	(let ((endMatchingLoop nil))
	  (while (progn (setq patPos (if (not patPos) (1- patLen) (1- patPos) ))
			(and (not endMatchingLoop) (>= patPos 0)))
	    (when (/= (-charCodeAt pattern patPos)
		      (-charCodeAt text (+ startPos patPos)))
	      (setq skip (- patPos (aref right (-charCodeAt text (+ startPos patPos)))))
	      (when (< skip 1) (setq skip 1))
	      (setq endMatchingLoop 1) ) ) )
	(when (= 0 skip) (setq result startPos)) ) )
    (if result result txtLen) ) )

;;(bm_substring_search "aabb" "zzaabbe")
