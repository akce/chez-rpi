;; SPDX-License-Identifier: Unlicense
;;
;; Chez scheme userspace/client library for Linux GPIO character device interface.
;;
;; BCM GPIO numbering (only) is used in this client.
;;
;; Akce: 2019.
;;
;; See linux userspace include file:
;;   <linux/gpio.h>	(typically /usr/include/linux/gpio.h)
;;
;; Presentation and slides by Linus Walleij, creator of the GPIO chardev interface:
;; https://www.youtube.com/watch?v=lQRCDl0tFiQ
;; http://elinux.org/images/9/9b/GPIO_for_Engineers_and_Makers.pdf
;;
;; Example C clients:
;; https://github.com/torvalds/linux/tree/master/tools/gpio
(library (rpi gpio)
  (export
   gpio-chip-open
   gpio-get-chip-info
   gpio-get-line-info
   make-gpiohandle-data free-gpiohandle-data
   gpio-get-linehandle
   gpio-get gpio-set!
   gpio-event-watch gpio-read-event
   close

   GPIO_HANDLE_REQUEST_INPUT GPIO_HANDLE_REQUEST_OUTPUT GPIO_HANDLE_REQUEST_ACTIVE_LOW GPIO_HANDLE_REQUEST_OPEN_DRAIN GPIO_HANDLE_REQUEST_OPEN_SOURCE

   GPIO_EVENT_REQUEST_RISING_EDGE GPIO_EVENT_REQUEST_FALLING_EDGE GPIO_EVENT_REQUEST_BOTH_EDGES)
  (import
   (except (chezscheme) read)
   (rpi ftypes-util))

  (define load-libc
    (load-shared-object "libc.so.6"))

  (c_funcs
   ;; int open(const char* pathname, int flags);
   (open	(string int)				int)
   ;; int close(int fd);
   (close	(int)					int)
   ;; int ioctl(int fd, unsigned long request, void* argp);
   (ioctl	(int unsigned-long void*)		int)
   ;; ssize_t read(int fd, void *buf, size_t count);
   (read	(int (* gpioevent-data) size_t)		ssize_t))

  (define O_RDRW	2)
  (define O_SYNC	#x101000)
  (define O_FLAGS	(bitwise-ior O_RDRW O_SYNC))

  (define-ftype char*32 (array 32 unsigned-8))
  (define-ftype gpiochip-info
    (struct
     [name	char*32]
     [label	char*32]
     [lines	unsigned-32]))

  (bitmap gpio-line-flag
    [GPIO_LINE_FLAG_KERNEL	0]
    [GPIO_LINE_FLAG_IS_OUT	1]
    [GPIO_LINE_FLAG_ACTIVE_LOW	2]
    [GPIO_LINE_FLAG_OPEN_DRAIN	3]
    [GPIO_LINE_FLAG_OPEN_SOURCE	4])

  (define-ftype gpioline-info
    (struct
     [line-offset	unsigned-32]
     [flags		unsigned-32]
     [name		char*32]
     [consumer		char*32]))

  (bitmap gpio-handle-request-flags
    [GPIO_HANDLE_REQUEST_INPUT		0]
    [GPIO_HANDLE_REQUEST_OUTPUT		1]
    [GPIO_HANDLE_REQUEST_ACTIVE_LOW	2]
    [GPIO_HANDLE_REQUEST_OPEN_DRAIN	3]
    [GPIO_HANDLE_REQUEST_OPEN_SOURCE	4])

  (define-ftype u32*64 (array 64 unsigned-32))
  (define-ftype u8*64 (array 64 unsigned-8))

  (define-ftype gpiohandle-request
    (struct
     [line-offsets	u32*64]
     [flags		unsigned-32]
     [default-values	u8*64]
     [consumer-label	char*32]
     [lines		unsigned-32]
     [fd		int]))

  (define-ftype gpiohandle-data
    (struct
     [values		u8*64]))

  (enum gpio-event-request
    [GPIO_EVENT_REQUEST_RISING_EDGE	#b01]
    [GPIO_EVENT_REQUEST_FALLING_EDGE	#b10]
    [GPIO_EVENT_REQUEST_BOTH_EDGES	#b11])

  (define-ftype gpioevent-request
    (struct
     [line-offset	unsigned-32]
     [handle-flags	unsigned-32]
     [event-flags	unsigned-32]
     [consumer-label	char*32]
     [fd		int]))

  ;; GPIO event types.
  (enum gpio-event-event
    [GPIOEVENT_EVENT_RISING_EDGE	#x01]
    [GPIOEVENT_EVENT_FALLING_EDGE	#x02])

  (define-ftype gpioevent-data
    (struct
     [timestamp		unsigned-64]
     [id		unsigned-32]))

  ;; IOCTLs are defined in terms of (type, number, size).
  ;; These values were created using util/defines.c
  ;; For info on how IOCTLs are encoded (ie, _IOWR), see <asm-generic/ioctl.h>
  ;; _IOR(0xb4, 0x01, struct gpiochip-info)
  (define GPIO_GET_CHIPINFO_IOCTL #x8044b401)
  ;; _IOWR(0xb4, 0x02, struct gpioline-info)
  (define GPIO_GET_LINEINFO_IOCTL #xc048b402)
  ;; _IOWR(0xb4, 0x03, struct gpiohandle-request)
  (define GPIO_GET_LINEHANDLE_IOCTL #xc16cb403)
  ;; _IOWR(0xb4, 0x04, struct gpioevent-request)
  (define GPIO_GET_LINEEVENT_IOCTL #xc030b404)
  ;; _IOWR(0xb4, 0x08, struct gpiohandle-data)
  (define GPIOHANDLE_GET_LINE_VALUES_IOCTL #xc040b408)
  ;; _IOWR(0xb4, 0x09, struct gpiohandle-data)
  (define GPIOHANDLE_SET_LINE_VALUES_IOCTL #xc040b409)

  (define gpio-chip-open
    (lambda (path)
      (open path O_FLAGS)))

  (define gpio-get-chip-info
    (lambda (fd)
      (alloc ([v &v gpiochip-info])
        (ioctl fd GPIO_GET_CHIPINFO_IOCTL v)
        (list (u8*->string (ftype-pointer-address (ftype-&ref gpiochip-info (name) &v)))
              (u8*->string (ftype-pointer-address (ftype-&ref gpiochip-info (label) &v)))
              (ftype-ref gpiochip-info (lines) &v)))))

  (define gpio-get-line-info
    (lambda (fd line)
      (alloc ([v &v gpioline-info])
        (ftype-set! gpioline-info (line-offset) &v line)
        (ioctl fd GPIO_GET_LINEINFO_IOCTL v)
        (list (ftype-ref gpioline-info (line-offset) &v)
              (ftype-ref gpioline-info (flags) &v)
              (u8*->string (ftype-pointer-address (ftype-&ref gpioline-info (name) &v)))
              (u8*->string (ftype-pointer-address (ftype-&ref gpioline-info (consumer) &v)))))))

  (define gpiohandle-offsets-set!
    (lambda (bufptr line-values)
      (let loop ([s 0] [vs line-values])
        (cond
         [(null? vs) bufptr]
         [else
          (ftype-set! gpiohandle-request (line-offsets s) bufptr (car vs))
          (loop (fx+ s 1) (cdr vs))]))))

  (define u8*64-set!
    (lambda (bufptr line-values)
      (let loop ([s 0] [vs line-values])
        (cond
         [(null? vs) bufptr]
         [else
          (ftype-set! u8*64 (s) bufptr (car vs))
          (loop (fx+ s 1) (cdr vs))]))))

  (define make-gpiohandle-data
    (lambda line-values
      (let* ([data (foreign-alloc (ftype-sizeof gpiohandle-data))]
             [&data (make-ftype-pointer gpiohandle-data data)])
        (u8*64-set! (ftype-&ref gpiohandle-data (values) &data) line-values))))

  (define free-gpiohandle-data
    (lambda (ghd)
      (unlock-object ghd)
      (foreign-free (ftype-pointer-address ghd))))

  (define strcpy
    (lambda (bufptr string)
      (let ([bv (string->utf8 string)])
        (for-each
         (lambda (i)
          (ftype-set! char*32 (i) bufptr (bytevector-u8-ref bv i)))
         (iota (bytevector-length bv)))
        ;; null terminate and return.
        (ftype-set! char*32 ((bytevector-length bv)) bufptr (char->integer #\nul))
        bufptr)))

  (define decode-line-defs
    (lambda (line-defs)
      ;; TODO assert (not (null? line-defs)).
      (let loop ([offsets '()] [vals '()] [defs line-defs])
        (cond
         [(null? defs)
          (values (reverse offsets) (reverse vals))]
         [(pair? (car defs))
          (let ([p (car defs)])
            (loop (cons (car p) offsets) (cons (cdr p) vals) (cdr defs)))]
         [else
          (loop (cons (car defs) offsets) (cons 0 vals) (cdr defs))]))))

  (define gpio-get-linehandle
    (lambda (chip-fd consumer-label line-flags . line-defs)
      (let-values ([(line-offsets line-values) (decode-line-defs line-defs)])
        (alloc ([data &data gpiohandle-request])
          (ftype-set! gpiohandle-request (fd) &data chip-fd)
          (ftype-set! gpiohandle-request (lines) &data (length line-offsets))
          (ftype-set! gpiohandle-request (flags) &data line-flags)
          (gpiohandle-offsets-set! &data line-offsets)
          (strcpy (ftype-&ref gpiohandle-request (consumer-label) &data) consumer-label)
          (when (fx=? (bitwise-and line-flags GPIO_HANDLE_REQUEST_OUTPUT) GPIO_HANDLE_REQUEST_OUTPUT)
            (u8*64-set! (ftype-&ref gpiohandle-request (default-values) &data) line-values))
          (ioctl chip-fd GPIO_GET_LINEHANDLE_IOCTL data)
          (ftype-ref gpiohandle-request (fd) &data)))))

  (define gpio-get
    (lambda (line-handle count)
      (alloc ([data &data gpiohandle-data])
        (ioctl line-handle GPIOHANDLE_GET_LINE_VALUES_IOCTL data)
        ;; convert to a number list.
        (let loop ([i 0] [acc '()])
          (cond
           [(fx<? i count)
            (loop (fx+ i 1) (cons (ftype-ref gpiohandle-data (values i) &data) acc))]
           [else
            (reverse acc)])))))

  (define gpio-set!
    (lambda (line-handle . line-values)
      (alloc ([data &data gpiohandle-data])
        (u8*64-set! &data line-values)
        (ioctl line-handle GPIOHANDLE_SET_LINE_VALUES_IOCTL data))))

  (define gpio-event-watch
    (lambda (chip-fd label lineflags eventflags line)
      (alloc ([data &data gpioevent-request])
        (ftype-set! gpioevent-request (line-offset) &data line)
        (ftype-set! gpioevent-request (handle-flags) &data lineflags)
        (ftype-set! gpioevent-request (event-flags) &data eventflags)
        (strcpy (ftype-&ref gpioevent-request (consumer-label) &data) label)
        (ioctl chip-fd GPIO_GET_LINEEVENT_IOCTL data)
        (ftype-ref gpioevent-request (fd) &data))))

  (define event-id->symbol
    (lambda (id)
      (cond
       [(fx=? id GPIOEVENT_EVENT_RISING_EDGE)	'RISING_EDGE]
       [(fx=? id GPIOEVENT_EVENT_FALLING_EDGE)	'FALLING_EDGE]
       [else	'UNKNOWN])))

  (define gpio-read-event
    (lambda (event-fd)
      (alloc ([data &data gpioevent-data])
        (read event-fd &data (ftype-sizeof gpioevent-data))
        (list
         (ftype-ref gpioevent-data (timestamp) &data)	; timestamp is in nanoseconds.
         (event-id->symbol (ftype-ref gpioevent-data (id) &data)))))))
