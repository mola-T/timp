;;; thread-socket.el --- Manage thread data sending  -*- lexical-binding: t; -*- 
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
;; Manage threads data.
;; It quenes up inbound and outbound data to prevent data loss
;; when transfering data concurrently across threads.
;;
;;; code:
;;; Code:
(require 'eieio-base)
(require 'fifo)
(require 'signal)


(defsignal thread-socket--outbound-signal
  ;; Singal to be emitted when there is outbound job.
  "Private variable. Modifying it may cause serious problem.")


(defsignal thread-socket--inbound-signal
  ;; Singal to be emitted when there is inbound job.
  "Private variable. Modifying it may cause serious problem.")


;; thread.socket is a singleton class
;; It helps quening inbound and outbound jobs.
;; When there is thread jobs or reply from child thread.
;; They are pushed to the socket instead of process directly.
;; Because if the jobs are processed directly,
;; the main thread will be in blocking state.
;; When another job is going to send out or received at the same moment,
;; the main thread cannot process it which cause data/job lost.
;; Quening is a very fast process which can be done almost at negligible time.
;; So, it ensures only one job is processing while it can quening up other jobs.
(defclass thread.socket (fifo eieio-singleton)
  ((type
    :initarg :type
    :accessor thread.socket.type
    :protection :private)
   ;; type = 'parent or 'child
   (outbound
    :initform nil
    :accessor thread.socket.outbound
    :protection :private)
   ;; buffer = fifo outbound data buffer
   ;; In parent, outbound contains a thread object
   ;; which the thread object has a job quene itself
   ;; In child, outbound contains thread.socket object
   (inbound
    :initform nil
    :accessor thread.socket.inbound
    :protection :private)
   ;; buffer = fifo inbound data buffer
   ;; Inbound contains unprocessed form of thread.packet object
   (buffer
    :initform nil
    :accessor thread.socket.buffer
    :protection :private)
   ;; For parent thread only
   ;; When thread object fails to send out thread job
   ;; because thread child thread has not yet reply
   ;; The thread object is stored here instead of requene immediately
   ;; This prevents flooding of thread-socket--outbound-signal
   ;; just for trying to send out job and requening
   (LDquene
    :initform nil
    :accessor thread.socket.LDquene
    :protection :private)
   ;; It is the large data quene.
   ;; When child threads have large data (>20kb) to send to parent,
   ;; they will ask for permission from the parent thread.
   ;; The parent thread will allow only one large data to be sent at the same time.
   ;; Other large data request will be placed here to quene for the premission.
   )
   "thread.socket class.")



(defconst thread--socket-instance
  (make-instance 'thread.socket
                 :type (if noninteractive 'child 'parent))
  ;; Initialize a thread.socket instance
  "Private variable. Modifying it may cause serious problem.")


(defun thread.socket.get ()
  ;; Return the singleton socket
  ;; Seems quite useless......
  ;; May have to remove it.
  "Private function. Using it may cause serious problem."
  thread--socket-instance)





(defun thread.socket.inbound.push (data)
  ;; Push DATA to thread.socket buffer.
  "Private function. Using it may cause serious problem."
  (fifo-push thread--socket-instance 'inbound data)
  (signal-emit 'thread-socket--inbound-signal))

(defun thread.socket.inbound.pop ()
  ;; Get DATA from thread.socket buffer.
  "Private function. Using it may cause serious problem."
  (fifo-pop thread--socket-instance 'inbound))

(defmethod thread.socket.getInbound ((obj thread.socket))
  ;; Get inbound buffer from socket
  ;; Only for the hasNext function
  "Private function. Using it may cause serious problem."
  (thread.socket.inbound obj))

(defun thread.socket.inbound.hasNext ()
  ;; Whether there is job in buffer
  "Private function. Using it may cause serious problem."
  (when (thread.socket.getInbound thread--socket-instance) t))





(defun thread.socket.outbound.push (data)
  ;; Push DATA to thread.socket buffer.
  "Private function. Using it may cause serious problem."
  (fifo-push thread--socket-instance 'outbound data)
  (signal-emit 'thread-socket--outbound-signal))

(defun thread.socket.outbound.pop ()
  ;; Get DATA from thread.socket buffer.
  "Private function. Using it may cause serious problem."
  (fifo-pop thread--socket-instance 'outbound))

(defmethod thread.socket.getOutbound ((obj thread.socket))
  "Private function. Using it may cause serious problem."
  ;; Get Outbound buffer from socket
  ;; Only for the hasNext function
  (thread.socket.outbound obj))

(defun thread.socket.outbound.hasNext ()
  ;; Whether there is job in buffer
  "Private function. Using it may cause serious problem."
  (when (thread.socket.getOutbound thread--socket-instance) t))



(defmethod thread.socket.getBuffer ((obj thread.socket))
  ;; Get the socket buffer.
  "Private function. Using it may cause serious problem."
  (thread.socket.buffer obj))

(defmethod thread.socket.addToBuffer ((obj thread.socket) thread)
  ;; Add the thread to buffer if it is not here.
  "Private function. Using it may cause serious problem."
  (cl-pushnew (thread.socket.buffer obj) thread))

(defmethod thread.socket.removeFromBuffer ((obj thread.socket) thread)
  ;; Get the thread from buffer and remove it the thread form buffer.
  "Private function. Using it may cause serious problem."
  (setf (thread.socket.buffer obj) (remq thread (thread.socket.buffer obj))))

(defun thread.socket.buffer.add (thread)
  ;; Add the thread to thread.scoket's buffer if it is not here.
  "Private function. Using it may cause serious problem."
  (thread.socket.addToBuffer thread--socket-instance thread))

(defun thread.socket.buffer.remove (thread)
  ;; Remove the thread from thread.scoket's buffer if it exists.
  "Private function. Using it may cause serious problem."
  (thread.socket.removeFromBuffer thread--socket-instance thread))

(defun thread.socket.isInBuffer (thread)
  ;; Return t if thread is in thread.socket.buffer.
  "Private function. Using it may cause serious problem."
  (when (memq thread (thread.socket.getBuffer thread--socket-instance)) t))


(defun thread.socket.LDquene.push (thread)
  ;; When child thread sends large data request
  ;; and it is not ready to handle,
  ;; push the child thread to the large-data-quene.
  "Private function. Using it may cause serious problem."
  (fifo-push thread--socket-instance 'LDquene thread))

(defun thread.socket.LDquene.pop ()
  ;; Pop the first thread in the LDquene.
  "Private function. Using it may cause serious problem."
  (fifo-pop thread--socket-instance 'LDquene))

(defun thread.socket.LDquene.first ()
  ;; Get the first thread in the LDquene without removing it.
  "Private function. Using it may cause serious problem."
  (fifo-first thread--socket-instance 'LDquene))

(defun thread.socket.LDquene.hasNext ()
  ;; Return whether there is job in the LDquene
  "Private function. Using it may cause serious problem."
  (when (fifo-first thread--socket-instance 'LDquene) t))




(provide 'thread-socket)
;;; thread-socket.el ends here
