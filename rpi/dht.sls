;; SPDX-License-Identifier: Unlicense
;;
;; Written by Akce 2019.
;;
;; Abandon.
;; The first few bits are missed on my rpi2 in dht-read between setting RUN mode and the call to read-temperature-humidity.
;; To fix, gpio line fd's would probably need to be bi-directional. That would also simplify this code greatly.
;;
;; Keep for now as an example of how to use the GPIO chardev interface. At the least it can get the temperature.. Some of the time..
(library (rpi dht)
  (export
   dht-open dht-close dht-read
   dht-label
   dht-start-duration)
  (import
   (chezscheme)
   (rpi dht-common)
   (rpi ftypes-util)
   (rpi gpio)
   (rpi poll))

  ;; [parameter] dht-label: GPIO consumer label for the DHT line.
  ;; [default]: "dht"
  (define dht-label
    (make-parameter "dht"))

  ;; MCU -> DHT Start signal (LO) is at least 1ms duration.
  (define dht-start-duration
    (make-parameter 1
      (lambda (x)
        (make-time 'time-duration (* x 1000 1000) 0))))

  ;; [procedure] set-WAIT-state!: Set DHT to WAIT (low-power-consumption-mode).
  ;; [return] gpio OUTPUT/HI file descriptor.
  (define set-WAIT-state!
    (lambda (chip-fd line)
      ;; Data bus's free status is HI and sets the DHT in low power mode. (See datasheet: pg5.)
      ;; TODO store result/wait-fd in a parameter?
      ;; TODO should we use the GPIO in ACTIVE LO mode?
      ;; Note that we don't close the linefd after setting OUTPUT/HI as the gpio chardev driver would free the resources and reset the line to INPUT/LO.
      (gpio-get-linehandle chip-fd (dht-label) GPIO_HANDLE_REQUEST_OUTPUT `(,line . 1))))

  (define dht-open
    (lambda (chip-fd line)
      (set-WAIT-state! chip-fd line)))

  (define dht-close
    (lambda (dht-fd)
      (close dht-fd)))

  ;; [procedure] dht-read: read temperature & humidity from DHT sensor.
  ;; [return] (dht-fd . (temperature . humidity))
  (define dht-read
    (lambda (chip-fd dht-fd line)
      (with-interrupts-disabled
       ;; Enter RUNNING state: response from the DHT should be within 20-40us.
       (set-RUNNING-state! dht-fd)
       ;; The GPIO must be re-opened in INPUT mode, then we can receive measurements from the DHT device.
       (close dht-fd)
       ;; Watch FALLING_EDGE only as we seem to catch more of those than RISING_EDGE.
       (let ([fd (gpio-event-watch chip-fd (dht-label) GPIO_HANDLE_REQUEST_INPUT GPIO_EVENT_REQUEST_FALLING_EDGE line)])
         (let ([readings (read-temperature-humidity fd)])
           (close fd)
           (values (set-WAIT-state! chip-fd line) readings))))))

  ;; Transition DHT from WAIT to RUNNING mode.
  ;; ie, Send start signal - LO for at least 1ms.
  ;; MCU -> DHT start protocol is described in the datasheet: pg5-7.
  (define set-RUNNING-state!
    (lambda (dht-fd)
      (gpio-set! dht-fd 0)
      (sleep (dht-start-duration))
      (gpio-set! dht-fd 1)))

  (define read-temperature-humidity
    (lambda (fd)
      (alloc ([pfd &pfd pollfd])
        (ftype-set! pollfd (fd) &pfd fd)
        (ftype-set! pollfd (events) &pfd POLLIN)
        (ftype-set! pollfd (revents) &pfd 0)
        (let loop ([rs '()])
          (let ([res (poll &pfd 1 1)])
            (cond
             [(fx>? res 0)	; data waiting to be read from fd.
              (loop (cons (gpio-read-event fd) rs))]
             [(zero? res)	; timeout: finished.
              (parse-data (reverse rs))]
             [else		; error
              (display "read-data error\n")]))))))
  )
