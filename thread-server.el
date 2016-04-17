;;; thread-server.el --- Implementation of child thread  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2015-2016 Mola-T
;; Author: Mola-T <Mola@molamola.xyz>
;; URL: https://github.com/mola-T/thread
;; Version: 1.0
;; Keywords: internal, lisp, processes, tools
;;
;;; License:
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;;; Commentary:
;;
;; It is the backend or child thread of thread.el
;; Under normal situation, end user may not notice that it is running.
;; Because the only propriate used is called by thread.el
;; and runs in a subprocess.
;;
;;; code:


;; Must not impletment socket in threadS
;; With previous testing, it will become 3 times slower.

;; In reality, don't need to require these package
;; Because they are controlled in parent thread
;; I put here just for debugging
;; (require 'fifo)
;; (require 'sign)
;; (require 'thread-packet)

(defvar threadS-stop nil
  "Thread will stop if it is set to t.")

(defconst threadS-stream "threadS-stream"
  "Process name of the data stream.
Implement through localhost.")

(defconst threadS-sender "threadS-sender"
  "Process name of the data sender.")

(defconst threadS-Dsender "threadS-Dsender"
  "Process name of the large data sender.")

(defvar threadS-id nil
  "Number of this thread.")

(defvar threadS-port nil
  "Port of the thread server (this server).")

(defvar threadS-parent-port nil
  "Port of thre parent thread.")

(defvar threadS-parent-large-data-port nil
  "Large data port of thre parent thread.")

(defvar threadS-large-data-permission nil
  "It becomes t when it gets permission to send large data")

(defvar threadS-buffer '(0)
  ;; Minimum need to have a list for nconc 
  "Buffer for imcomplete received data.")

(defvar threadS-complete-packet-buffer nil
  "threadS-buffer is transferred here when getting a complete packet.
So threadS-buffer is able to handle one more incoming packet.
Usually there is only one packet to receive at the same time.
However, there is an expectional case that it needs to receive
a large data sending premission.")

(defsign threadS-quit-signal
  "A block signal to be emitted when it receives
a quit message from parent thread.")


(defun threadS-init ()

  "Ininialize and listen to main process for instruction."
  
  ;; Get parent port
  (let ((info (read-from-minibuffer "" nil nil t)))
    (setq threadS-id (car info)
          threadS-parent-port (cadr info)
          threadS-parent-large-data-port (car (cddr info))))
  ;; I tried to use `caddr' and it won't work.
  ;; I used more than an hour to figure out it is `caddr' causing the problem
  ;; because it is in cl-lib....
  ;; ╭∩╮（￣.￣）╭∩╮

  ;; Start stream for listening data
  ;; in terms of network process
  ;; Should be the faster way to get data from another process
  (make-network-process :name threadS-stream
                        :server t
                        :host 'local
                        :service t
                        :family 'ipv4
                        :filter 'threadS-receive-data
                        :nowait t)

  ;; Store the local server port that need to send back to parent
  (setq threadS-port (process-contact (get-process threadS-stream) :service))

  ;;! This sleep time is very important
  ;;! In parent thread, it is doing:
  ;; >>>>> Register the thread to the thread--record
  ;; >>>>> (setf (cdr (assoc thread-num thread--record)) thread)
  ;; threadS-send-system-data need to do after parent thread has finished this operation
  ;; accept-process-output seems a `sleep-for' in batch-mode
  ;; Only accept-process-output is blocking the program without using cpu power
  (accept-process-output nil 0.1)
  (threadS-send-port-data threadS-port)

  ;; Redirect 'message to parent's log
  (advice-add 'message :around 'threadS-message)
  
  (while (null threadS-stop)
    (sleep-for 0.5)))





(defun threadS-receive-data (proc data)

  "Process received data."
  ;; It needs to be very efficient.
  ;; As it fails to do so, parent process will be blocking
  ;; just for waiting it for process receiving data.

  ;; Data will arrived as string.
  ;; Large data will be split into small data chunks at parent process.
  ;; A newline charater "\n" indicates the end of the chunks.
  ;; One chunk is sent at a time.
  ;; Depends on OS, the max. data size for a data chunk is fixed, say 4kb for my PC.
  ;; So data chunk is put in thread-buffer first.
  ;; And combine to form a complete data when the newline character is met.

  ;;; Debug only
  ;; (prin1 data)
    
  ;; Check only the last character
  (if (string-match "\n" data (- (length data) 1))
      (progn
        (nconc threadS-buffer (list data))
        (setq threadS-complete-packet-buffer threadS-buffer) ;; Move the completed packet to another buffer
        (setq threadS-buffer (list 0)) ;; Need to use (list 0) instead of '(0)
        (threadS-process-data)) 
    ;; nconc is 100 times faster than concat a string
    (nconc threadS-buffer (list data))))





(defun threadS-process-data ()

  "Process a complete data from thread-buffer."
  ;; It won't block the parent process.
  ;; Efficiency is not care. XD
    
  ;; Combine the list
  (let ((string (mapconcat 'identity (cdr threadS-complete-packet-buffer) ""))
        packet
        packet-type)
    ;; Cut the last newline char
    (setq string (substring string 0 (- (length string) 1)))
    (setq packet (read string))

    ;; Distrubute jobs
    (when (thread.packet-p packet)
      (setq packet-type (thread.packet.getType packet))
      (cond
       ((eq packet-type 'exe)
        (threadS-exe-packet-handler packet))
       ((eq packet-type 'code)
        (threadS-code-packet-handler packet))
       ((eq packet-type 'quit)
        (threadS-quit))
       ((eq packet-type 'ldr)
        (setq threadS-large-data-permission t))))))
  
  






(defun threadS-send-data (packet)

  "When `thread-socket--outbound-signal' is emitted in the socket,
this function is invoked to really send out data through the network stream."

  ;; There is a need to seperate large datas
  ;; Because the main thread can't handle large data at the same time
  ;; resulting a packet (the real ip packet) buffer overflow
  ;; I am not sure it is due to too many retry counts or buffer overflow
  ;; Anyway, there will be packet loss if large data is not handled specially

  ;; To make it more effieicent, small packets (<20b) can go out directly
  ;; while large packets (>20kb) will need to do a handshake will the main thread
  ;; The main thread holds a quene to let only one large data to be sent at one time
  
  (let ((data (concat (prin1-to-string packet) "\n"))
        sender)

    (if (> (length data) 20000)
        (progn
          (while (null (setq sender (ignore-errors
                                      (open-network-stream threadS-sender
                                                           nil
                                                           "localhost"
                                                           threadS-parent-port
                                                           'plain))))
            (accept-process-output nil 0.05))

          ;; Send a handshake packet in the normal data stream
          (process-send-string sender
                               (concat
                                (prin1-to-string (make-instance 'thread.packet
                                                                :source threadS-id
                                                                :type 'ldr
                                                                :data (length data)))
                                "\n"))

                    
          (ignore-errors (delete-process sender))

          ;; Wait until premission got
          (while (null threadS-large-data-permission)
            (accept-process-output nil 0.05))

          ;; Set through large data channel
          (while (null (setq sender (ignore-errors
                                (open-network-stream threadS-Dsender
                                                     nil
                                                     "localhost"
                                                     threadS-parent-large-data-port
                                                     'plain)))))
          (process-send-string sender data)
          (delete-process sender)
          (setq threadS-large-data-permission nil))

      ;; Some data go out directly
      (while (null (setq sender (ignore-errors
                                  (open-network-stream threadS-sender
                                                       nil
                                                       "localhost"
                                                       threadS-parent-port
                                                       'plain))))
        (accept-process-output nil 0.05))
      (process-send-string sender data)
      (delete-process sender))))


(defun threadS-send-port-data (data)

  "Send port number back to parent thread."

  (threadS-send-data
   (make-instance 'thread.packet
                  :source threadS-id
                  :type 'port
                  :data data)))


(defun threadS-send-err-data (&optional error-code error-handler data)

  "Send reply back to parent thread."
  
  (threadS-send-data
   (make-instance 'thread.packet
                  :source threadS-id
                  :type 'err
                  :error-handler error-handler
                  :data (and error-handler (cons error-code data)))))


(defun threadS-send-msg-data (data)

  "Send message back to parent thread."

  (threadS-send-data
   (make-instance 'thread.packet
                  :source threadS-id
                  :type 'msg
                  :data data)))


(defun threadS-send-quit ()

  "Send signal to parent thread that it is safe to quit."
  
  (threadS-send-data
   (make-instance 'thread.packet
                  :source threadS-id
                  :type 'quit
                  :data t)))


(defun threadS-send-rpy-data (&optional reply-func data)

  "Send reply back to parent thread."

  (threadS-send-data
   (make-instance 'thread.packet
                  :source threadS-id
                  :type 'rpy
                  :reply reply-func
                  :data (and reply-func (list data)))))


(defun threadS-send-tgi-data (function data)

  "Send instruction generated by child thread."
  
  (threadS-send-data
   (make-instance 'thread.packet
                  :source threadS-id
                  :type 'tgi
                  :reply function
                  :data data)))




(defun threadS-exe-packet-handler (packet)

  "Execute the instruction issued from the parent thread.
It replies with the returning result of the execution to the parent thread.
Otherwise, it will reply nil.
If there is any error during the execution of instrustion,
a packet will be sent to notify the error."

  (let ((data (thread.packet.getData packet))
        (reply-func (thread.packet.getReply packet))
        (error-handler (thread.packet.getErrorHandler packet))
        error-info
        result)

        (setq result (condition-case list
                     (apply (car data) (cdr data))
                   (error list
                          (setq error-info list))))
        
    (if error-info
        (threadS-send-err-data (car data) error-handler error-info)
      (threadS-send-rpy-data reply-func result))))



(defun threadS-code-packet-handler (packet)

  "Evaluate the code issued from the parent thread.
It replies with the returning result of the evaluation to the parent thread.
Otherwise, it will reply nil.
If there is any error during the evaluation of code,
a packet will be sent to notify the error."
  
  (let ((code (thread.packet.getData packet))
        (reply-func (thread.packet.getReply packet))
        (error-handler (thread.packet.getErrorHandler packet))
        error-info
        result)
        
    (setq result (condition-case list
                     (eval code)
                   (error list
                          (setq error-info list))))
    
    (if error-info
        (threadS-send-err-data code error-handler error-info)
      (threadS-send-rpy-data reply-func result))))







(defun threadS-message (orig-func &rest args)

  "Message is meaningless in child thread.
  So send it back to parent."

  (let ((message (ignore-errors (apply 'format args))))
    (when message
      (threadS-send-msg-data message))))


(defun threadS-set-load-path (path)
  "Set load path."
  (setq load-path path))

(defun threadS-require-packet (&rest packets)
  "Require package."
  (dolist (packet (car packets))
    (require packet)))

(defun threadS-quit ()
  "Terminate the thread safely by emit a signal.
Any backend packages should make connection to this signal
if they want to quit safely."
  (emitB threadS-quit-signal)
  (threadS-send-quit))





(defun threadS-do-nothing ()
  "Seriously, this is a function doing nothing.
If you can read this documentation, you are most possibily looking at
the source code. So, this function is just put here to say hi to you!"
  (when (fboundp 'you-are-looking-at-the-source-code)
    'say-hello))


(defun threadS-debug-write-file (&rest datas)

  "These function is for debugging.
When developing this package, if anything goes wrong,
the subprocess just closes directly or does not respond.
I put this function everywhere to find at which moment it goes wrong."

  (let (string)
    (dolist (data datas)
      (if (stringp data)
          (setq string (concat string data "\n"))
        (setq string (concat string (prin1-to-string data) "\n"))))
    (write-region string
                  nil
                  (concat (file-name-as-directory user-emacs-directory) (number-to-string (or threadS-id 9999)))
                  t)))

;; This provide package is just for developer
;; This package is never needed to be require.
(provide 'thread-server)
;;; thread-server.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
