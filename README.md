# Thread

The first multithreading elisp library.

## Requirement

* Emacs 24
* Netowrk adaptor

## Dependency

* fifo.el - https://github.com/mola-T/fifo
* sign.el - https://github.com/mola-T/sign

## Demonstration

______________________________
<br>

## Basic Usage

In this section, I will introduce the basic functions to let you create a simple multithread program.

##### Require this library

```elisp
(require 'thread)
;; Just add this to the top of your package...
```

##### `(thread.validate object)`

This is the first function you need to know. It tells you two information:

1. whether object is a thread
2. if object is a thread, whether it is valid.

Valid means you can push jobs to the thread.

```elisp
(thread.validate "a")
;; ==> nil
;; because "a" is not a thread

(setq a (thread.get))
;; thread.get is to create a new thread.
;; It will be covered soon
(thread.validate a)
;; ==> t
;; Now a is a valid thread.

(thread.quit a)
;; thread.quit is to close a thread.
;; It will be covered soon
(thread.validate a)
;; ==> nil
;; Now a is NOT a valid thread.
```

##### `(thread.get &key name persist)`

`thread.get` is the *ONLY* function that creates a thread and returns the thread.

The `name` argument let you to give the name to the thread.
In fact, it does nothing but to let you identitfy the thread when calling
`list-process' of `process-list'. Personlly, I always ignore it.

The `persist` argument is very important.
It stated whether the thread should be persisted.
If it is nil, after a single instruction, the thread quits automatically.
If it is t, the thread persists and you are responsible for quiting the thread
either by `thread.quit`(better) or `thread.forceQuit`."

```elisp
(setq my-thread (thread.get))
;; Obtain a non-persist thread.

(thread.validate my-thread)
;; ==> t

(thread.send.exec my-thread 'message "Hello world.")
;; thread.send.exec is to send an instruction to the thread.
;; It will be covered soon.

(thread.validate my-thread)
;; ==> nil
```

```elisp
(setq my-thread (thread.get :persist t))
;; Obtain a persist thread.

(thread.validate my-thread)
;; ==> t

(thread.send.exec my-thread 'message "Hello world.")
;; thread.send.exec is to send an instruction to the thread.
;; It will be covered soon.

(thread.validate my-thread)
;; ==> t

(thread.quit my-thread)
(thread.validate my-thread)
;; ==> nil
```


##### `(thread.send.exec thread function &rest arguments &key......)`

This is the first function (or two) allowing you to send jobs to threads.
You can see there are `keys` in the end. I would leave them for now.
Without the `keys` we can still have pretty functional threads.

From now on, I will ignore `(setq my-thread (thread.get :persist t))` and `(thread.quit my-thread)`
in all examples assuming that you know they are always there.

`thread` is the thread object you request from `thread.get`.

`function` is the function to be execute in child thread.

`arguments` are the arguments provided to `function`.

```elisp
(thread.send.exec my-thread 'message "Emacs finally have %s in %d" "multithreading" 2016)
;; ==> t
;; ==> Emacs finally have multithreading in 2016
;;
;; The first return t, let you know the job has been put into the thread.
;; However, it does not guarantee a return. Just to let you know the job will be processed.
;;
;; When does it return a nil?
;;
;; (thread.quit my-thread)
;; (thread.send.exec my-thread  'message "Emacs finally have %s in %d" "multithreading" 2016)
;; ==> nil
```

Sending single instruction repeatedly is sometimes trouble.
You can use lambda forom in `function`

```elisp
(thread.send.exec my-thread (lambda ()
                              (make-directory "~/testdirectory")
                              (make-directory "~/testdirectory2")
                              (make-directory "~/testdirectory3")))

;; ==> t
;; It creates 3 directories in your home folder.
```

What if I make an error in the instruction?

```elisp
(thread.send.exec my-thread 'message "I provide" " many " "arguments to message")
;; ==>t
;; nothing happen
```

##### `(thread.send.code thread &key code ....other-keys)`

This is another function let you to send instruction to child thread.
Still, there is other keys we will leave it now.
It works very similar to `thread.send.exec` expect it sends code.

```elisp
(thread.send.code my-thread
                  :code `(progn
                           (message "Emacs is great now.")
                           (message "Oh Emacs is always great.")))

;; ==> t
;; ==> Emacs is great now.
;; ==> Oh Emacs is always great.
```
All othre properties are the same as `thread.send.exec`.

##### `(thread.quit thread)`

This function performs a safe quit on the thread. A safe quit means it allows you to finish the current job and the job quenes.
After that, it will emit a quit signal in the child thread. Finally, the thread is being quit.

You can think of it as you choose `Shut down` in your computer so as to let your computer to closing jobs.

##### `(thread.foreQuit thread)`

This function performs a force quit on the thread.
You can think of it as you remove the power from your computer to turn it off.
The thread is promised to be closed.

<br>

________________________________________


## How it works?

Before we move on to a more advanced usage, I think it is better to let you know how `thread` works.

Emacs is not a multithreading program. It is impossible to create a real multithreading library under a single threaded program.
So `thread` is not a multithreading library in precise. `thread` is a multi-process library pretended as a multithread library.
A multi-process impiles that every thread you create is in a seperated and independent process. They have no shared memory.
When you do `(setq a 1)` in parent thread, `a` is not defined in all threads.

What `thread` is doing is simply opening instances of Emacs (the real nature of child threads).
The main instance (the parent thread) acts as a controller to quene up jobs, distribute jobs, send instruction to threads and receive returned data from threads.
Since there is no shared memory, how data is transferred is a big problem. `thread` achieves by transferring data through localhost.
This is why you need a network adaptor for this package.

Although `thread` is not a real multithread library, it does perform very close to multithread behaviour.

<br>
___________________________________


## Advance usage

##### `(thread.send.exec thread function &rest arguments &key unique reply-func error-handler quit-warn)`
##### `(thread.send.code thread &key code unique reply-func error-handler quit-warn)`

There are four keys for the functions.

* `reply-func`

You may notice that I always use `message` in previous sections.
It is because `message` is a special function in `thread` (cover later).
You have not yet handled any return values.

The `reply-func` is the function to be called when value is returned.

```elisp
(defun my-handle-string-list (string-list)
    (message (car (last string-list))))
;; A function print the last item of a string list.

(thread.send.exec my-thread
                  'mapcar 'number-to-string (number-sequence 1 100)
                  :reply-func 'my-handle-string-list)

;; ==> 100
;; The string list is created in child thread and returned.
;; After returned, it will be passed to my-handle-string-list
;; so that 100 is print.
```

If you want to debug what is returned, you can pass the returned object to `thread-debug-print`.
It will print the return object to *thread log* buffer.

* `error-handler`

Did I say that there is nothing happen if you send buggy codes to child threads?
It is not 100% correct. The errors are just ignored.
The error is catched and can be returned.

```elisp
(thread.send.exec my-thread
                  'what-is-this-function-doing
                  1 2 3 4 5 6 7 8 9 10
                  :error-handler 'thread-debug-print)

;; ==> t
;; t just mean job is quened up and will be processed.
;; In *thread log*
;; (what-is-this-function-doing void-function what-is-this-function-doing)

(thread.send.exec my-thread
                  (lambda ()
                    (mapcar 'identity 1 2 3 4 5))
                    :error-handler 'thread-debug-print)

;; ==> t
;; In *thread log*
ll ((lambda nil (mapcar (quote identity) 1 2 3 4 5)) wrong-number-of-arguments mapcar 6)
```

* `quit-warn`

`quit-warn` accepts a string.
Because child threads may be doing valuable work, if user tried to quit Emacs when the thread is working, it may damage the data.
Honestly, nothing can be done to prevent user to quit Emacs if they want.
Therefore, `quit-warn` just accepts a string to prompt for user confirmation to quit.
If user still want to quit, the thread is forced quit.

```elisp
(thread.send.exec my-thread
                  'writing-large-files-to-harddisk
                  :quit-warn "Quitting emacs may lead to data loss in writing-large-files-to-harddisk.")

;; When user try to quit, they are prompted for confirm:
;; Quitting emacs may lead to data loss in writing-large-files-to-harddisk.
;; Do you really want to quit?
```

* `unique`

Some jobs take times but don't need to perform repeatedly.
For example, you have a database in the child thread and you write an interactive function to let the user upadate the database.
The update process may take a minute. It is non-sense if many update requests are sent to child thread while it is upadating.
You don't want the same job to quene up in the thread. So you can pass `:unique t` to tell `thread` that this job is unique. 

<br>

___________________________________


## More advance usage

In fact, the six functions introduced is everything for `thread`. This part give you a hint to play develop a multithread package.

##### Packages working in the dark

Sending complex instruction or codes by `thread.send.exec` or `thread.send.code` is sometimes quite confusing.
Especially, when you need to deal with variables, determining whether the variables should be evaluated in child thread side or parent thread side.
Also, you may have nested backquote when you are sending codes.
This is ant-ihuman to have such complex codes just for sending correct variables to child thread.
When you come to this point, you may find this advice valuable.

*Why not make things easier?*

The designed of `thread.send.exec` and `thread.send.code` is not for sending complex code. What I expect is I can send a simple command to do complexthings and return simple result. For example, you want to remove all "*.elc" files in a directory, you should just tell the child thread 'Hey, delete all *.elc, tell me when finished.' But not 'Hey, go to folder x. Get all file contents. It they have .elc, delete it. Go to subfolder a.......".

To achieved this you may need to seperate the implementation of you code.

First, create a dark package, say `my-delete-file.el`

```elisp
(defun my-delete-files (file-type)
  ;; codes implements here
  )

(provide 'my-delete-files)
```

In your main package:

```elsip
(thread.send.exec my-thread
                  (lambda ()
                    ;; implementation here
                    ;; it is over complex
                    ;; and so ugly.
                    ))
                    
(thread.requirePackage my-thread 'my-delete-files)
(thread.send.exec my-thread 'my-delete-files "elc")
;; Dose it look better?
```

##### `(thread.requirePackage thread &rest package)`

Oh yeah, another function. This is not something new. You have many experiences using `(require 'package)`.
This is exactly the same thing. Remember I told you before threads are actually separate process.
So `load-path` in child threads are not well prepared. This function manage to provide the `load-path` as well as `require` a package in the child thread.


When you come to the dark, let me introduce to you two more functions.

##### `(threadS-send-tgi-data function data)`

There is no typo. `threadS-x` functions are functions in `thread-server.el` which is working in the dark.

When your package go dark, you may need to bribe this package. In your dark package, requiring `thread-server` is optionally. Because `thread-sever` is always loaded (otherwise, `thread.get` fails).
Even if you required this package, you will get many compile warnings, because the dark doesn't follow the rule.

OK. We come back to `threadS-send-tgi-data`. `tgi` stands fo thread generated instruction.
In your dark package, you may have functions monitoring something, for example file changes. This is quite easy to be done by a timer. When you have new information in the dark, how to send back to the parent thread? Right, by using this function. As usual, it is not difficult.

* `function` is the function in the parent thread to be called.
* `data` is the argument pass to `function`

You can see it as a dark version of `thread.send.exec`.


##### `(threadS-sleep-for second)`

`sleep-for` behaves differently in the dark. `(threadS-sleep-for second)` is doing exactly the same as `(sleep-for second)`.

##### `(threadS-debug-write-file &rest datas)`

One more function. This function is so special that I hope you never need to use it. This is the last resort to debug package in the dark.
When developing `thread`, in the early stage, there is no way to debug easily. If any things go wrong in the dark, the sub-process just close clicly.
Or don't notify the parent thread for any errors.
I used this function everywhere to find when did it go wrong and what is the problem.
This function make text file in the home directory to write out data.

After the `error-handler` has been finished, I seldom use it.
