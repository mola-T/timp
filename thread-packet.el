;;; thread-packet.el --- data packet to transfer between threads  -*- lexical-binding: t; -*- 
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
;; It provides thread.packet class which is used to transfer
;; instrustion or data between parent threads and child threads.
;;
;;; code:
(require 'eieio)

(defconst thread--packet-type
  '(port err msg quit exe code rpy tgi ldr)
  ;; Type of packet.
  ;; Stand for:
  ;; port = port
  ;; err = error
  ;; msg = message
  ;; quit = quit
  ;; exe = single execution instruction
  ;; code = code
  ;; rpy = reply
  ;; tgi = thread generated instruction
  ;; ldr = large data request
  "Private variable. Modifying it may cause serious problem.")

(defun thread--packet-type-p (test)
  ;; Predicate whether TEST is a correct thread type.
  "Private function. Using it may cause serious problem."
  (memq test thread--packet-type))


(defclass thread.packet ()
  ((source :initarg :source
           :type integer
           :accessor thread.packet.source
           :protection :private)
   ;; The id of thread whose send this packet
   ;; For parent thread, it is not really important
   ;; For child thread, it is very important as parent thread
   ;; needs this info to differentiate which child thread is replying
   (type :initarg :type
         :type (satisfies thread--packet-type-p)
         :accessor thread.packet.type
         :protection :private)
   ;; port err msg exe cod rpy
   ;; Stand for:
   ;; port = port
   ;; err = error
   ;; msg = message
   ;; quit = quit
   ;; exe = single execution instruction
   ;; code = code
   ;; rpy = reply
   ;; tgi = thread generated instruction
   ;; ldr = large data request
   (data :initarg :data
         :initform nil
         :accessor thread.packet.data
         :protection :private)
   ;; Store the data
   (reply :initarg :reply
          :initform nil
          :accessor thread.packet.reply
          :protection :private)
   ;; Store the function that will be called when the job is done and repiled.
   ;; The function will be called with argument stored in data
   ;; where data is the return value of the job
   (error-handler :initarg :error-handler
                  :initform nil
                  :accessor thread.packet.errorHandler
                  :protection :private)
   ;; The function to be called if error occure in child thread.
   ;; The function need to accept the error infomation will be
   ;; passed to the handler function
   (quit-warn :initarg :quit-warn
              :initform nil
              :accessor thread.packet.quitWarn
              :protection :private)
   ;; The message to be printed and asked for confirmation
   ;; if the thread is tried to force quit by user.
   )
  ;; Once a packet is created, it cannot be modified.
  "A thread packet class.")


(defmethod initialize-instance :before ((obj thread.packet) &rest args)
  "Constructor. Make sure source and type get initialized."
  (unless (plist-get (car args) ':source)
    (error "Slot :source must be initialized."))
  (unless (plist-get (car args) ':type)
    (error "Slot :type must be initialized.")))


(defmethod thread.packet.getSource ((obj thread.packet))
  ;; Get the source thread of the packet.
  "Private function. Using it may cause serious problem."
  (thread.packet.source obj))

(defmethod thread.packet.getType ((obj thread.packet))
  ;; Get the type of the packet.
  "Private function. Using it may cause serious problem."
  (thread.packet.type obj))

(defmethod thread.packet.getData ((obj thread.packet))
  ;; Get the Data of the packet.
  "Private function. Using it may cause serious problem."
  (thread.packet.data obj))

(defmethod thread.packet.getReply ((obj thread.packet))
  ;; Get the reply form packet
  "Private function. Using it may cause serious problem."
  (thread.packet.reply obj))

(defmethod thread.packet.getErrorHandler ((obj thread.packet))
  ;; Get the error handler form packet
  "Private function. Using it may cause serious problem."
  (thread.packet.errorHandler obj))

(defmethod thread.packet.getQuitWarn ((obj thread.packet))
  ;; Get the quit warning message from packet
  "Private function. Using it may cause serious problem."
  (thread.packet.quitWarn obj))

(defmethod thread.packet.clrQuitWarn ((obj thread.packet))
  ;; Get the quit warning message from packet
  "Private function. Using it may cause serious problem."
  (setf (thread.packet.quitWarn obj) nil))

(provide 'thread-packet)
;;; thread-packet.el ends here
