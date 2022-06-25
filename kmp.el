
(defun kmp_compile_pattern (pattern)
  "Compile pattern to DFA."

  (defun create-2d-array (x y init)
    (let ((arr1 (make-vector x nil)))
      (dotimes (i x)
	(aset arr1 i (make-vector y init)) )
      arr1 ) )
  
  (let* ((patLen (length pattern))
	 (R 256)
         (restartPos 0)
	 (dfa (create-2d-array R patLen 0)))
    
    (aset (aref dfa (elt pattern 0)) 0 1)

    (let ((patPos 0))
      (while (progn (setq patPos (1+ patPos)) (< patPos patLen))
	(dotimes (c R)
	  (aset (aref dfa c) patPos (aref (aref dfa c) restartPos)) )
	
	(aset (aref dfa (elt pattern patPos)) patPos (1+ patPos))
	(setq restartPos
	      (aref (aref dfa (elt pattern patPos)) restartPos) )


	;;(print (format "::::::%s" (aref dfa patPos)))
	(print (format "patPos: %s,  restartPos: %s" patPos restartPos))
	
	)
      )
    dfa )
  )


(let* ((pattern "bcebcebcebe") (dfa (kmp_compile_pattern pattern)) (line ""))
  (print (format "Pattern: %s" pattern))
  (dotimes (idx (length pattern))
    (setq line (concat line (format "%d: " idx)))
    (dotimes (i 5)
      (let ((ch (+ i 97)) )
	(setq line (concat line (format "%c=>%d " ch (aref (aref dfa ch) idx))))
	)
      )
    (print line)
    (setq line "")
    )
  )

(defun kmp_search (pattern text)
  (let ((dfa (kmp_compile_pattern pattern)))

    (let ((textPos 0) (patPos 0) (N (length text)) (M (length pattern)))
      (while (and (< textPos N) (< patPos M))
	(setq patPos (aref (aref dfa (elt text textPos)) patPos))
	(setq textPos (1+ textPos)) )
      
      (if (= patPos M) (- textPos M) N ) ) ) )


(kmp_search "abe" "abcabe")

    ;; ;;
    ;; (let [iAm (atom 0) jAm (atom 0) N (count text) M (count pattern)]
    ;; (while (and (< @iAm N) (< @jAm M))
    ;;   (reset! jAm (aget dfa (-charCodeAt text @iAm) @jAm))
    ;;   (swap! iAm inc) )
    ;; (if (= @jAm M)
    ;;   (- @iAm M)
    ;;   N ) )
