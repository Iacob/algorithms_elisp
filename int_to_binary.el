
(defun int-to-binary (val)
  (let ((x val) (result ""))
    (while (> x 0)
      (setq result (concat (number-to-string (% x 2)) result))
      (setq x (/ x 2)))
    result))

(message "5 => %s" (int-to-binary 5))
(message "50 => %s" (int-to-binary 50))
(message "9000 => %s" (int-to-binary 9000))