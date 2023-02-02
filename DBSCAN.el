
(let ()
  (defun dbscan (points radius min-pts)
    (let ((v-points nil)
	  (fn-map-import-points nil)
	  (fn-distance nil)
	  (fn-search-reachable nil)
	  (fn-append-pts-reachable nil)
	  (fn-mark-point-as-core nil)
	  (fn-save-reachable nil)
	  (fn-mark-point-as-grouped nil)
	  (fn-group-points nil)
	  (fn-group-all-points nil)
	  )
      (setq fn-map-import-points
	    (lambda ()
	      (seq-map-indexed (lambda (point idx) (list 'id (1+ idx) 'v point)) points)))
      
      (setq fn-distance
	    (lambda (p1 p2)
	      (abs (- (plist-get p1 'v) (plist-get p2 'v)))))

      (setq fn-search-reachable
	    (lambda (point)
	      (seq-filter (lambda (pt)
			    (<= (funcall fn-distance point pt) radius))
			  v-points) ) )

      (setq fn-append-pts-reachable
	    (lambda ()
	      (let ((pts nil))
		(cl-loop for pt in v-points do
			 (setq pts (funcall fn-search-reachable pt))
			 (setq pts (seq-map (lambda (p) (plist-get p 'id)) pts))
			 (setq pts (seq-filter (lambda (p) (/= p (plist-get pt 'id))) pts))
			 (nconc pt (list 'pts pts))
			 )
		)
	      
	      ))

      (setq fn-mark-core-points
	    (lambda ()
	      (cl-loop for point in v-points do
		       (when (>= (length (plist-get point 'pts)) min-pts)
			 (nconc point (list 'c 't)))) ) )

      (setq find-point
	    (lambda (id)
	      (seq-find (lambda (p) (= (plist-get p 'id) id)) v-points)))

      (setq fn-group-points
	    (lambda (start-point)
	      (if (null start-point)
		  '()
		(progn
		  (let ((point-list nil))
		    (when (and (plist-get start-point 'c)
			       (not (plist-get start-point 'g)))
		      (nconc start-point (list 'g 't))
		      (setq point-list (list start-point))
		      (let ((pts (plist-get start-point 'pts)))
			(cl-loop for p-id in pts do
				 (let ((pt (funcall find-point p-id)))
				   (nconc point-list
					(funcall fn-group-points pt))
				   )
				 )
			)
		      )
		    point-list
		    )
		  )
		)
	      ))

      (setq fn-group-all-points
	    (lambda ()
	      (let ((has-core-point 't)
		    (point-groups '()))
		(while has-core-point
		  (let ((point nil)
			(point-list nil))
		    (setq point (seq-find (lambda (pt) (and (plist-get pt 'c) (not (plist-get pt 'g)))) v-points))
		    (if (not point)
			(setq has-core-point nil)
		      (progn
			(setq point-list
			      (funcall fn-group-points point))
			(if point-groups
			    (nconc point-groups (list point-list))
			  (setq point-groups (list point-list)))) ) ) )
		;;point-groups
		(mapcar (lambda (group)
			  (mapcar (lambda (l1)
				    (plist-get l1 'v)) group)) point-groups)
		) ) )
      
      (setq v-points (funcall fn-map-import-points))

      ;;(funcall fn-search-reachable (nth 1 v-points))
      (funcall fn-append-pts-reachable)
      (funcall fn-mark-core-points)
      ;;(message "%s" v-points)
      ;;(funcall fn-group-all-points)
      ;;(message "%s" (nth 1 v-points))
      ;;(funcall fn-group-points (nth 1 v-points))
      (cl-loop for group in (funcall fn-group-all-points) do
	       (message "%s" group))
      )
    )

  (dbscan '(0.5 1 1.1 1.2 2 2.8 3 3.1) 0.5 2)
  )



;; Output:

;; (1 1.1 1.2)
;; (2.8 3 3.1)

