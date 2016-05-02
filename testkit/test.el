;; Test require package
;; problem
(progn
  (require 'timp)
  (setq testthread (timp-get :persist t))
  (timp-require-package testthread 'smartkey 'helm)
  (timp-send-code testthread
                    :code
                    `(progn
                       (message "Load-path: %s" load-path)
                       (message "Features: %s" features)))
  ;; Should reply the child thread's load path and features.
  (timp-quit testthread))




;; Test sending codes and reply function
(progn
  (require 'timp)
  (setq testthread (timp-get :persist t))
  (timp-send-code testthread
                    :code
                    `(progn
                       (message "hello")
                       (make-string 50 ?a)
                       (make-string 50 ?b))
                    :reply-func 'timp-debug-print)
  ;; Should print "bbbb..." to thread log
  (timp-quit testthread))




;; Test sending executable and rely function
(progn
  (require 'timp)
  (setq testthread (timp-get :persist t))
  (timp-send-exec testthread
                    (lambda ()
                      (let ((a 10)
                            (b 20))
                        (+ a b)))
                    :reply-func 'timp-debug-print)
  ;; Should print 30 to thread log
  (timp-quit testthread))



;; Test error handler
(progn
  (require 'timp)
  (setq testthread (timp-get :persist t))
  (timp-send-exec testthread
                    'format
                    "Hello world %s %s" "only me, lack one arg...."
                    :error-handler 'timp-debug-print)
  ;; Should print the error to thread log
  (timp-quit testthread))




;; Test unique job
(progn
  (require 'timp)
  (setq testthread (timp-get :persist t))
  (dotimes (var 10)
    (timp-send-exec testthread
                      (lambda ()
                        (let ((time (current-time)))
                          (while (< (time-to-seconds (time-subtract (current-time) time)) 5)
                            t))
                        (message "DONE"))
                      :unique t))
  ;; Only one done should be print to thread log
  (timp-quit testthread))



;; Test quit warning - thread
(progn
  (require 'timp)
  (setq testthread (timp-get :persist t :quit-warn "this is a thread quit warning..."))
  (timp-send-exec testthread
                    (lambda ()
                      (let ((time (current-time)))
                        (while (< (time-to-seconds (time-subtract (current-time) time)) 7)
                          t))
                      (message "DONE"))
                    :reply-func 'timp-debug-print
                    :error-handler 'timp-debug-print)
  (save-buffers-kill-emacs))

(timp-quit testthread)


;; Test quit warning - job
(progn
  (require 'timp)
  (setq testthread (timp-get :persist t))
  (timp-send-exec testthread
                    (lambda ()
                      (let ((time (current-time)))
                        (while (< (time-to-seconds (time-subtract (current-time) time)) 7)
                          t))
                      (message "DONE"))
                    :quit-warn "Thread is working, really quit? "
                    :reply-func 'timp-debug-print
                    :error-handler 'timp-debug-print)
  (save-buffers-kill-emacs))

(timp-quit testthread)


;; Test quit warning - job and thread
(progn
  (require 'timp)
  (setq testthread (timp-get :persist t :quit-warn "this is a thread quit warning..."))
  (timp-send-exec testthread
                    (lambda ()
                      (let ((time (current-time)))
                        (while (< (time-to-seconds (time-subtract (current-time) time)) 7)
                          t))
                      (message "DONE"))
                    :reply-func 'timp-debug-print
                    :error-handler 'timp-debug-print
                    :quit-warn "Thread is working, really quit? ")
  (save-buffers-kill-emacs))

(timp-quit testthread)


;; Test concurrency
(progn
  (require 'timp)
  (setq testthread1 (timp-get :persist t))
  (setq testthread2 (timp-get :persist t))
  (setq testthread3 (timp-get :persist t))
  (setq testthread4 (timp-get :persist t))
  (setq testthread5 (timp-get :persist t))
  (switch-to-buffer-other-window timp-debug-buffer-name)
  (dotimes (var 1000)
    (timp-send-exec testthread1 'format "    %d" var :reply-func 'timp-debug-print)
    (timp-send-exec testthread2 'format "         %d" var :reply-func 'timp-debug-print)
    (timp-send-exec testthread3 'format "              %d" var :reply-func 'timp-debug-print)
    (timp-send-exec testthread4 'format "                   %d" var :reply-func 'timp-debug-print)
    (timp-send-exec testthread5 'format "                       %d" var :reply-func 'timp-debug-print))
  (timp-quit testthread1)
  (timp-quit testthread2)
  (timp-quit testthread3)
  (timp-quit testthread4)
  (timp-quit testthread5))




;; Test sending large data concurrently
;; In my computer, it freezed for about 35s to send 640mb data
;; 20160418 - 10:57:44AM $ 20000000
;; 20160418 - 10:57:45AM $ 20000000
;; 20160418 - 10:57:58AM $ 200000000
;; 20160418 - 10:58:09AM $ 200000000
;; 20160418 - 10:58:19AM $ 200000000
;; It's about 18mb per second
(progn
  (require 'timp)
  (setq testthread1 (timp-get :persist t))
  (setq testthread2 (timp-get :persist t))
  (setq testthread3 (timp-get :persist t))
  (setq testthread4 (timp-get :persist t))
  (setq testthread5 (timp-get :persist t))
  (switch-to-buffer-other-window timp-debug-buffer-name)
  (timp-send-exec testthread1 'length (make-string 200000000 ?a) :reply-func 'timp-debug-print) ;; 200mb string
  (timp-send-exec testthread2 'length (make-string 200000000 ?a) :reply-func 'timp-debug-print) ;; 200mb string
  (timp-send-exec testthread3 'length (make-string 200000000 ?a) :reply-func 'timp-debug-print) ;; 200mb string
  (timp-send-exec testthread4 'length (make-string 20000000 ?a)  :reply-func 'timp-debug-print) ;; 20mb string
  (timp-send-exec testthread5 'length (make-string 20000000 ?a) :reply-func 'timp-debug-print) ;; 20mb string
  (timp-quit testthread1)
  (timp-quit testthread2)
  (timp-quit testthread3)
  (timp-quit testthread4)
  (timp-quit testthread5))



;; Test receiving large data
(progn
  (require 'timp)

  (defun test-receive-string (string)
    (timp-debug-print
     (format "String length received: %d" (length string))))

  (setq testthread (timp-get :persist t))
  (switch-to-buffer-other-window timp-debug-buffer-name)
  (timp-send-exec testthread 'make-string 200000000 ?a :reply-func 'test-receive-string)  ;; 200mb string
  (timp-quit testthread))

;; Test receiving large data -2
;; Can't wait until it finish....
(progn
  (require 'timp)

  (defun test-receive-string (string)
    (timp-debug-print
     (format "String length received: %d" (length string))))

  (setq testthread (timp-get :persist t))
  (switch-to-buffer-other-window timp-debug-buffer-name)
  (timp-send-exec testthread 'make-string 200000000 ?a :reply-func 'test-receive-string)
  (timp-quit testthread)) ;; 2000mb string




;; Test receiving large data concurrently - 30kb
;; No data lost!
(progn
  (require 'timp)

  (defun test-receive-string (string)
    (timp-debug-print
     (format "String length received: %d" (length string))))

  (setq testthread1 (timp-get :persist t))
  (setq testthread2 (timp-get :persist t))
  (setq testthread3 (timp-get :persist t))
  (setq testthread4 (timp-get :persist t))
  (setq testthread5 (timp-get :persist t))
  (switch-to-buffer-other-window timp-debug-buffer-name)
  (timp-send-exec testthread1 'make-string 30000 ?a :reply-func 'test-receive-string) ;; 30kb string
  (timp-send-exec testthread2 'make-string 30000 ?a :reply-func 'test-receive-string) ;; 30kb string
  (timp-send-exec testthread3 'make-string 30000 ?a :reply-func 'test-receive-string) ;; 30kb string
  (timp-send-exec testthread4 'make-string 30000 ?a :reply-func 'test-receive-string) ;; 30kb string
  (timp-send-exec testthread5 'make-string 30000 ?a :reply-func 'test-receive-string) ;; 30kb string
  (timp-quit testthread1)
  (timp-quit testthread2)
  (timp-quit testthread3)
  (timp-quit testthread4)
  (timp-quit testthread5))


;; Test receiving large data concurrently - 300kb
;; No data lost!!
(progn
  (require 'timp)

  (defun test-receive-string (string)
    (timp-debug-print
     (format "String length received: %d" (length string))))

  (setq testthread1 (timp-get :persist t))
  (setq testthread2 (timp-get :persist t))
  (setq testthread3 (timp-get :persist t))
  (setq testthread4 (timp-get :persist t))
  (setq testthread5 (timp-get :persist t))
  (switch-to-buffer-other-window timp-debug-buffer-name)
  (timp-send-exec testthread1 'make-string 300000 ?a :reply-func 'test-receive-string) ;; 300kb string
  (timp-send-exec testthread2 'make-string 300000 ?a :reply-func 'test-receive-string) ;; 300kb string
  (timp-send-exec testthread3 'make-string 300000 ?a :reply-func 'test-receive-string) ;; 300kb string
  (timp-send-exec testthread4 'make-string 300000 ?a :reply-func 'test-receive-string) ;; 300kb string
  (timp-send-exec testthread5 'make-string 300000 ?a :reply-func 'test-receive-string) ;; 300kb string
  (timp-quit testthread1)
  (timp-quit testthread2)
  (timp-quit testthread3)
  (timp-quit testthread4)
  (timp-quit testthread5))


;; Test receiving large data concurrently - 2mb
;; Data loss!!!!
(progn
  (require 'timp)

  (defun test-receive-string (string)
    (timp-debug-print
     (format "String length received: %d" (length string))))

  (setq testthread1 (timp-get :persist t))
  (setq testthread2 (timp-get :persist t))
  (setq testthread3 (timp-get :persist t))
  (setq testthread4 (timp-get :persist t))
  (setq testthread5 (timp-get :persist t))
  (switch-to-buffer-other-window timp-debug-buffer-name)
  (timp-send-exec testthread1 'make-string 2000000 ?a :reply-func 'test-receive-string) ;; 2mb string
  (timp-send-exec testthread2 'make-string 2000000 ?a :reply-func 'test-receive-string) ;; 2mb string
  (timp-send-exec testthread3 'make-string 2000000 ?a :reply-func 'test-receive-string) ;; 2mb string
  (timp-send-exec testthread4 'make-string 2000000 ?a :reply-func 'test-receive-string) ;; 2mb string
  (timp-send-exec testthread5 'make-string 2000000 ?a :reply-func 'test-receive-string) ;; 2mb string
  (timp-quit testthread1)
  (timp-quit testthread2)
  (timp-quit testthread3)
  (timp-quit testthread4)
  (timp-quit testthread5))



;; Test receiving large data concurrently - 20mb
;; No data lost!!!!
(progn
  (require 'timp)

  (defun test-receive-string (string)
    (timp-debug-print
     (format "String length received: %d" (length string))))

  (setq testthread1 (timp-get :persist t))
  (setq testthread2 (timp-get :persist t))
  (setq testthread3 (timp-get :persist t))
  (setq testthread4 (timp-get :persist t))
  (setq testthread5 (timp-get :persist t))
  (switch-to-buffer-other-window timp-debug-buffer-name)
  (timp-send-exec testthread1 'make-string 20000000 ?a :reply-func 'test-receive-string) ;; 20mb string
  (timp-send-exec testthread2 'make-string 20000000 ?a :reply-func 'test-receive-string) ;; 20mb string
  (timp-send-exec testthread3 'make-string 20000000 ?a :reply-func 'test-receive-string) ;; 20mb string
  (timp-send-exec testthread4 'make-string 20000000 ?a  :reply-func 'test-receive-string) ;; 20mb string
  (timp-send-exec testthread5 'make-string 20000000 ?a :reply-func 'test-receive-string) ;; 20mb string
  (timp-quit testthread1)
  (timp-quit testthread2)
  (timp-quit testthread3)
  (timp-quit testthread4)
  (timp-quit testthread5))



;; Test receiving large data concurrently -640mb total
;; No Data loss!!!!
;; During receiving, it is not blocking!!!!!!!!!!!!!
(progn
  (require 'timp)

  (defun test-receive-string (string)
    (timp-debug-print
     (format "String length received: %d" (length string))))

  (setq testthread1 (timp-get :persist t))
  (setq testthread2 (timp-get :persist t))
  (setq testthread3 (timp-get :persist t))
  (setq testthread4 (timp-get :persist t))
  (setq testthread5 (timp-get :persist t))
  (switch-to-buffer-other-window timp-debug-buffer-name)
  (timp-send-exec testthread1 'message "hello" :reply-func 'test-receive-string)
  (timp-send-exec testthread1 'make-string 200000000 ?a :reply-func 'test-receive-string) ;; 200mb string
  (timp-send-exec testthread2 'make-string 200000000 ?a :reply-func 'test-receive-string) ;; 200mb string
  (timp-send-exec testthread3 'make-string 200000000 ?a :reply-func 'test-receive-string) ;; 200mb string
  (timp-send-exec testthread4 'make-string 20000000 ?a  :reply-func 'test-receive-string) ;; 20mb string
  (timp-send-exec testthread5 'make-string 20000000 ?a :reply-func 'test-receive-string) ;; 20mb string
  (timp-quit testthread1)
  (timp-quit testthread2)
  (timp-quit testthread3)
  (timp-quit testthread4)
  (timp-quit testthread5))



;; receiving data from 100 threads concurrently - 3kb
(progn
  (require 'timp)
  (defun test-receive-string (string)
    (timp-debug-print
     (format "String length received: %d" (length string))))
  (let (threads)
    (dotimes (var 100)
      (push (timp-get :persist t) threads))
    (switch-to-buffer-other-window timp-debug-buffer-name)
    (dolist (thread threads)
      (timp-send-exec thread 'make-string 3000 ?a :reply-func 'test-receive-string)) ;; 3kb string
    (dolist (thread threads)
      (timp-quit thread))))


;; receiving data from 100 threads concurrently - 100kb
(progn
  (require 'timp)
  (defun test-receive-string (string)
    (timp-debug-print
     (format "String length received: %d" (length string))))
  (let (threads)
    (dotimes (var 100)
      (push (timp-get :persist t) threads))
    (switch-to-buffer-other-window timp-debug-buffer-name)
    (dolist (thread threads)
      (timp-send-exec thread 'make-string 100000 ?a :reply-func 'test-receive-string)) ;; 100kb string
    (dolist (thread threads)
      (timp-quit thread))))

;; receiving data from 100 threads concurrently - 2mb
(progn
  (require 'timp)
  (defun test-receive-string (string)
    (timp-debug-print
     (format "String length received: %d" (length string))))
  (let (threads)
    (dotimes (var 100)
      (push (timp-get :persist t) threads))
    (switch-to-buffer-other-window timp-debug-buffer-name)
    (dolist (thread threads)
      (timp-send-exec thread 'make-string 2000000 ?a :reply-func 'test-receive-string)) ;; 2mb string
    (dolist (thread threads)
      (timp-quit thread))))




;; Performance
(defun my-rehash (input count)
  (dotimes (var count)
   (setq input (secure-hash 'sha512 input)))
  (message input))


;; single thread
;; (379.100353081 10693 235.00926678600018)
(insert
 (pp
  (benchmark-run
      (progn
        (require 'my-rehash)
        (my-rehash "1" 10000000) ;; Rehash itself 10m times
        (my-rehash "2" 10000000) ;; Rehash itself 10m times
        (my-rehash "3" 10000000) ;; Rehash itself 10m times
        (my-rehash "4" 10000000) ;; Rehash itself 10m times
        (my-rehash "5" 10000000) ;; Rehash itself 10m times
        (my-rehash "6" 10000000) ;; Rehash itself 10m times
        (my-rehash "7" 10000000) ;; Rehash itself 10m times
        (my-rehash "8" 10000000) ;; Rehash itself 10m times
        (my-rehash "9" 10000000) ;; Rehash itself 10m times
        (my-rehash "10" 10000000) ;; Rehash itself 10m times
        ))))(379.100353081 10693 235.00926678600018)


;; 13x second
;; 3 times faster while not blocking
(progn
  (require 'timp)
  (setq testthread1 (timp-get :persist t))
  (timp-send-exec testthread1 'format "Hello %s"  "World" :reply-func 'timp-debug-print)
  (timp-quit testthread1)
  (switch-to-buffer-other-window timp-debug-buffer-name)
  
  (setq threads nil)
  (dotimes (var 10)
    (push (timp-get :persist t) threads))
  (setq count 0)
  (dolist (thread threads)
    (timp-require-package thread 'my-rehash)
    (timp-send-exec thread 'my-rehash (number-to-string (setq count (1+ count))) 10000000 :reply-func 'timp-debug-print)
    (timp-quit thread)))



(process-list)
(dolist (pro (process-list))
  (delete-process pro))
