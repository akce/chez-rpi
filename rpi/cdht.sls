;; SPDX-License-Identifier: Unlicense
;;
;; Written by Akce 2019.
;;
;; A mostly C implementation of DHT sensor reading to see if there's much of a difference between the Chez scheme implementation.
;; There isn't on my test rpi2.
;;
;; Example usage:
;;
;; > (import (rpi cdht) (rpi gpio))
;; > (define chipfd (gpio-chip-open "/dev/gpiochip0"))
;; > (define d (dht-open chipfd 23))
;; > (dht-read d)
;; 22.8
;; >

(library (rpi cdht)
  (export
   dht-open dht-close dht-read
   dht-print)
  (import
   (chezscheme)
   (rpi dht-common)
   (rpi ftypes-util))

  (define lib-load
    (load-shared-object (locate-library-object "rpi/libdht.so")))

  (define-ftype dht
    (struct
     [chipfd	int]
     [linefd	int]
     [gpionum	int]))
  (define-ftype dht* void*)
  (define-ftype timestamps (array 90 unsigned-64))
  (define-ftype edge-types (array 90 int))

  (c_funcs
   ;; dht_t* dht_open(int chipfd, int gpio_num);
   (dht-open (int int) dht*)
   ;; int dht_read(dht_t* dht, u_int64_t* timestamps, int* types);
   (dht_read (dht* (* timestamps) (* edge-types)) int)
   (dht-print (dht*) void)
   ;; void dht_close(dht_t* dht);
   (dht-close (dht*) void))

  (define dht-read
    (lambda (d)
      (alloc ([ts &ts timestamps]
              [es &es edge-types])
        (let ([count (dht_read d &ts &es)])
          (cond
           [(fx<=? count 0)	; error
            #f]
           [else
            (parse-data
             (map
              (lambda (i)
                (cons
                 (ftype-ref timestamps (i) &ts)
                 (ftype-ref edge-types (i) &es)))
              (iota count)))])))))

  )
