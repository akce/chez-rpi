;; SPDX-License-Identifier: Unlicense
;;
;; Written by Akce 2019.
;;
;; Common routines between the cdht and dht libraries.
(library (rpi dht-common)
  (export
   parse-data
   edges->time-splits
   time-splits->bit-list
   split-at split-at*
   bit-list->number)
  (import (chezscheme))

  (define parse-data
    (lambda (data)
      (parse-temperature
       (map
        (lambda (rbyte-bit-list)
          (bit-list->number (reverse rbyte-bit-list)))
        (split-at*
         (reverse	; need to reverse because we may be missing some leading bits.
          (time-splits->bit-list
           (edges->time-splits data))) 8)))))

  (define parse-temperature
    (lambda (numbers)
      (let ([tintegral (list-ref numbers 2)]
            [tdecimal (list-ref numbers 3)])
        ;; TODO convert to negative time when 0x80 & tintegral == #t.
        (/ (bitwise-ior (bitwise-arithmetic-shift-left (bitwise-and tintegral #x7f) 8) tdecimal) 10.0))))

  (define split-at
    (lambda (lst n)
      (let loop ([lhs '()] [rhs lst] [i 0])
        (cond
         [(or (null? rhs) (fx=? i n))
          (values (reverse lhs) rhs)]
         [else
          (loop (cons (car rhs) lhs) (cdr rhs) (fx+ i 1))]))))

  (define split-at*
    (lambda (lst n)
      (let loop ([splits '()] [ls lst])
        (cond
         [(fx>? (length ls) n)
          (let-values ([(lhs rhs) (split-at ls n)])
            (loop (cons lhs splits) rhs))]
         [else
          (reverse (cons ls splits))]))))

  (define bit-list->number
    (lambda (bit-list)
      (fold-left
       (lambda (acc bit)
         ;;(display acc)(display " ")(display bit)(newline)
         (+ (* acc 2) bit))
       0 bit-list)))

  (define time-splits->bit-list
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
      ;(display splits)(newline)
      (map
       (lambda (x)
         ;; TODO check outside range for missed event?
         (cond
          #;[(fx<? x 70000)	#f]
          [(fx<? x 100000)	0]
          #;[(fx<? x 150000)	1]
          #;[else			#f]
          [else			1]))
       ;; TODO filter out leading sensor response.
       splits)))

  (define edges->time-splits
    (lambda (data)
      (let loop ([rs (cdr data)] [lhs (car data)] [splits '()])
        (cond
         [(null? rs)	(reverse splits)]
         [else		; Calculate time difference and push onto splits.
          (let ([rhs (car rs)])
            (loop (cdr rs) rhs (cons (- (car rhs) (car lhs)) splits)))]))))
  )
