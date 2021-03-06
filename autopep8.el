(with-no-warnings
   (require 'cl))

(defgroup autopep8 nil
  "autopep8 for python"
  :group 'languages)

(defcustom autopep8-command "autopep8"
  "The 'autopep8' command to use."
  :type 'string
  :group 'autopep8)

(defcustom autopep8-aggressive 2
  "The aggressiveness of autopep8."
  :type 'integer
  :options '(0 1 2)
  :group 'autopep8)

(defcustom autopep8-line-length 79
  "The maximum line length."
  :type 'integer
  :group 'autopep8)

(defun autopep8-before-save ()
  "Apply autopep8 to any python buffer before saving."
  (interactive)
  (when (eq major-mode 'python-mode) (autopep8)))

(defun autopep8-format-buffer ()
  "Formats the current buffer with autopep8."

  (interactive)
  (let ((tmpfile (make-temp-file "autopep8" nil ".py"))
        (patchbuf (get-buffer-create "*autopep8 patch*"))
        (errbuf (get-buffer-create "*autopep8 Errors*"))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8))

    (with-current-buffer errbuf
      (setq buffer-read-only nil)
      (erase-buffer))
    (with-current-buffer patchbuf
      (erase-buffer))

    (write-region nil nil tmpfile)

    (if (zerop (apply #'call-process (autopep8--commandline errbuf tmpfile)))
        (if (zerop (call-process-region (point-min) (point-max) "diff"
                                        nil patchbuf nil "-n" "-" tmpfile))
            (progn
              (kill-buffer errbuf)
              (message "Buffer is already pep8"))
          (autopep8--apply-rcs-patch patchbuf)
          (kill-buffer errbuf)
          (message "Applied autopep8"))
      (message "Could not apply autopep8. Check errors for details")
       (autopep8--process-errors (buffer-file-name) tmpfile errbuf))
    (kill-buffer patchbuf)
    (delete-file tmpfile)))

(defun autopep8--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current
buffer."
  (let ((target-buffer (current-buffer))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "invalid rcs patch or internal error in autopep8--apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (decf line-offset len)
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (autopep8--goto-line (- from line-offset))
                (incf line-offset len)
                (autopep8--delete-whole-line len)))
             (t
              (error "invalid rcs patch or internal error in autopep8--apply-rcs-patch")))))))))

(defun autopep8--commandline (errbuf tmpfile)
  ;; Build the autopep8 commandline.
  `(,autopep8-command nil ,errbuf nil ,@(make-list autopep8-aggressive
                                                   "--aggressive")
                      "--max-line-length" ,
                      (number-to-string autopep8-line-length) "--in-place" ,
                      tmpfile)
  )

(defun autopep8--goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

(defun autopep8--delete-whole-line (&optional arg)
  ;; Emacs uses both kill-region and kill-new, Xemacs only uses
  ;; kill-region. In both cases we turn them into operations that do
  ;; not modify the kill ring. This solution does depend on the
  ;; implementation of kill-line, but it's the only viable solution
  ;; that does not require to write kill-line from scratch.
  (cl-flet ((kill-region (beg end)
                      (delete-region beg end))
         (kill-new (s) ()))
    (kill-whole-line arg)))

(defun autopep8--process-errors (filename tmpfile errbuf)
  ;; Convert the autopep8 stderr to something understood by the compilation mode.
  (with-current-buffer errbuf
    (goto-char (point-min))
    (insert "autopep8 errors:\n")
    (while (search-forward-regexp (concat "^\\(" (regexp-quote tmpfile) "\\):") nil t)
      (replace-match (file-name-nondirectory filename) t t nil 1))
    (compilation-mode)
    (display-buffer errbuf)))

(provide 'autopep8)
(require 'autopep8)
