
(defconst base64-table "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defun base64-decode (b64)
  (let* ((parts (seq-partition b64 4))
         result-len
         result
         ;;(result (make-vector (* (length parts) 3) ?\s))
         (start-pos 0)
         current-byte-group
         fn-trans1 fn-trans2 fn-trans3)

    (if (< (length parts) 1)
        (setq result-len 0)
      (setq result-len
            (- (* (length parts) 3)
               (seq-count (lambda (x) (equal x ?=))
                          (seq-elt parts (1- (length parts)))))))
    
    (setq fn-trans1
          (lambda ()
            (let* ((char1 (aref current-byte-group 0))
                   (char2 (aref current-byte-group 1))
                   (part1 (seq-position base64-table char1))
                   (part2 (or (seq-position base64-table char2) 0)))
              (aset result
                    start-pos
                    (logior (ash part1 2) (ash part2 -4))))))
    
    (setq fn-trans2
          (lambda ()
            (let* ((char1 (aref current-byte-group 1))
                   (char2 (aref current-byte-group 2))
                   (part1 (seq-position base64-table char1))
                   (part2 (seq-position base64-table char2)))
              (when (and part1 part2)
                (aset result
                      (1+ start-pos)
                      (logior (ash (logand part1 #b00001111) 4)
                              (ash part2 -2)))))))
    
    (setq fn-trans3
          (lambda ()
            (let* ((char1 (aref current-byte-group 2))
                   (char2 (aref current-byte-group 3))
                   (part1 (seq-position base64-table char1))
                   (part2 (seq-position base64-table char2)))
              (when (and part1 part2)
                (aset result
                      (+ start-pos 2)
                      (logior (ash (logand part1 #b00000011) 6) part2))))))

    (when (> result-len 0)
      (setq result (make-vector result-len ?\s))
      (dolist (part1 parts)
        (setq current-byte-group part1)
        (funcall fn-trans1)
        (funcall fn-trans2)
        (funcall fn-trans3)
        (setq start-pos (+ start-pos 3))))
    
    result))



(message "%s" (concat (base64-decode "VG8gZXJyIGlzIGh1bWFuLCBidXQgdG8gcmVhbGx5IGZvdWwgdGhpbmdzIHVwIHlvdSBuZWVkIGEgY29tcHV0ZXIuCiAgICAtLVBhdWwgUi5FaHJsaWNo")))
