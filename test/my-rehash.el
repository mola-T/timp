(defun my-rehash (input count)
  (dotimes (var count)
   (setq input (secure-hash 'sha512 input)))
  (message input)
  input)

(provide 'my-rehash)
