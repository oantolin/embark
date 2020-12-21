;; rewrite embark-act along these lines
(defun mock-act ()
  (interactive)
  (let* ((overriding-terminal-local-map
          (make-composed-keymap embark-file-map embark-general-map))  
         (key (read-key-sequence "Action: "))
         (cmd (key-binding key)))
    (when (eq cmd 'execute-extended-command)
      (let (overriding-terminal-local-map)
        (setq cmd
              (condition-case nil
                  (read-extended-command)
                (quit nil)))))
    (when (eq cmd 'embark-keymap-help)
      (let* ((embark--keymap overriding-terminal-local-map)
             overriding-terminal-local-map)
        (setq cmd (embark--completing-read-map))))
    (if (null cmd)
        (minibuffer-message "Canceled")
      (let* ((target (run-hook-with-args-until-success 'embark-target-finders))
             overriding-terminal-local-map)
        (minibuffer-with-setup-hook
            (lambda ()
              (delete-minibuffer-contents)
              (insert target)
              (run-at-time 0 nil #'exit-minibuffer))
          (message "%s(%s)" cmd target)
          (command-execute cmd))))))
