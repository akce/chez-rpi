;; SPDX-License-Identifier: Unlicense
;;
;; Written by Akce 2019.
;;
;; Quick and dirty interface to the poll system call (Linux/Glibc only).

(library (rpi poll)
  (export
   poll wait-fd

   pollfd

   POLLIN POLLPRI POLLOUT POLLERR POLLHUP POLLNVAL)
  (import
   (chezscheme)
   (rpi ftypes-util))

  (define load-libc
    (load-shared-object "libc.so.6"))

  ;; <sys/poll.h> defines types thus.
  (define-ftype pollfd
    (struct
     [fd	int]
     [events	short]
     [revents	short]))
  (define-ftype nfds unsigned-long)

  ;; From glibc <bits/poll.h>
  (enum poll-events
    [POLLIN	#x01]
    [POLLPRI	#x02]
    [POLLOUT	#x04]
    ;; The following can occur in revents.
    [POLLERR	#x08]
    [POLLHUP	#x10]
    [POLLNVAL	#x20])


  (c_funcs
   ;; int poll(struct pollfd *fds, nfds_t nfds, int timeout);
   (poll ((* pollfd) nfds int) int))

  (define wait-fd
    (lambda (fd timeout)
      (alloc ([pfd &pfd pollfd])
        (ftype-set! pollfd (fd) &pfd fd)
        (ftype-set! pollfd (events) &pfd POLLIN)
        (ftype-set! pollfd (revents) &pfd 0)
        (poll &pfd 1 timeout))))
  )
