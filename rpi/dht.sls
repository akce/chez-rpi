;; SPDX-License-Identifier: Unlicense
;;
;; Written by Akce 2019.
;;
;; Abandon.
;; Half the readings are missed on my rpi2 in dht-read between setting RUN mode and the call to read-temperature-humidity.
;; To fix, gpio line fd's would probably need to be bi-directional. That would also simplify this code greatly.
;;
;; Keep for now as an example of how to use the GPIO chardev interface.
(library (rpi dht)
  (export
   edges->time-splits
   time-splits->bits
   dht-open dht-close dht-read
   dht-label
   dht-start-duration)
  (import
   (chezscheme)
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
              (parse-data rs)]
             [else		; error
              (display "read-data error\n")]))))))

  (define parse-data
    (lambda (data)
      (time-splits->bits (edges->time-splits data))))

  (define time-splits->bits
    (lambda (splits)
      ;; Sensor response:
      ;;  * HI 20-40us
      ;;  * LO 80us
      ;;  * HI 80us
      ;; So when watching only one EDGE type: 80us + 80us = 160us roughly.
      ;; Bit protocol:
      ;;  * Start transmit = LO 50us
      ;;  * Bit 0 = HI 26-28us
      ;;  * Bit 1 = HI 70us
      ;; So for one EDGE type: 0 = 50 + 28us, 1 = 50 + 70us roughly.
      (map
       (lambda (x)
         (if (fx<? x 82000) 0 1))
       ;; TODO filter out leading sensor response.
       splits)))

  (define edges->time-splits
    (lambda (rdata)
      (let loop ([rs (cdr rdata)] [odd (car rdata)] [splits '()])
        (cond
         [(null? rs)
          (when odd
            (display "warning: at least one missing reading..."))
          splits]
         [(pair? odd)	; Calculate time difference and push onto splits.
          (loop (cdr rs) #f (cons (- (car odd) (car (car rs))) splits))]
         [else
          (loop (cdr rs) (car rs) splits)]))))

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
  )
