(require 'timp)

(defun timp-test-send-variable (thread)
  (timp-send-variable thread load-path default-frame-alist))

(provide 'timp-send-variable)

