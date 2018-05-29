;;; transform-word.el -- transform word or region
;;; commentary:
;;; code:

(defun pfn-transform-word-or-region (fmt)
  "Transform word or words in a region according to FMT."
  (interactive "sFormat string: ")
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (bounds-of-thing-at-point 'symbol)))
         (text   (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (when bounds
      (delete-region (car bounds) (cdr bounds))
      (insert (mapconcat (function (lambda (x) (format fmt x)))
                         (split-string text) " ")))))

;;; transform-word.el ends here
