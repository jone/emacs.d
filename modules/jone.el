(defun jone-locate-first-dominating-file (patterns &optional startpath)
  "Look up the directory hirearchy and search for the first match of patterns
at the nearest location."
  (let ((startpath (or startpath buffer-file-name)))
    (first
     (-non-nil
      (mapcar (lambda (pattern)
                (let ((match (locate-dominating-file startpath pattern)))
                  (if match (concat match pattern))))
              patterns)))))


(defun jone-make-changelog-entry ()
  "Make a new changelog entry in the nearest changelog file found."
  (interactive)
  (let ((changelog (jone-locate-first-dominating-file '("docs/HISTORY.txt"
                                                        "CHANGELOG.txt")))
        (author (user-login-name)))

    (when changelog
      (find-file changelog)
      (goto-char (point-min))
      (forward-paragraph 2)

      ;; Remove "- Nothing changed yet", if it is there.
      (mark-paragraph)
      (replace-string "\n- Nothing changed yet.\n" "")

      (newline)
      (insert "- ")
      (insert (concat " [" author "]"))
      (newline)
      (forward-line -1)
      (beginning-of-line)
      (forward-char 2))))


(provide 'jone)
