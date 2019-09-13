;; Chez scheme Raspberry Pi GPIO library.
;;
;; Copyright (c) 2019 Akce. MIT licensed.
;;
;; NOTE: DEPRECATED!
;; NOTE: GPIO sysfs has been replaced by a GPIO character device interface. Use that instead.
;;
;; This is an attempt at a pure Chez implementation of GPIO control functions for a Raspberry Pi under a Raspbian based OS.
;;
;; Currently only BCM GPIO pin numbering is supported.
;;
;; Useful Raspberry Pi GPIO resources:
;;
;; https://www.raspberrypi.org/documentation/hardware/raspberrypi/gpio/README.md
;;
;;   Raspberry Pi foundation GPIO homepage. It describes the GPIOs and contains a number of useful links.
;;
;; https://www.raspberrypi.org/documentation/hardware/raspberrypi/bcm2835/BCM2835-ARM-Peripherals.pdf
;; https://elinux.org/BCM2835_datasheet_errata
;;
;;   BCM2835 Datasheet & Errata.
;;
;; https://elinux.org/RPi_Low-level_peripherals
;; https://elinux.org/RPi_GPIO_Code_Samples
;;
;; https://pypi.org/project/RPi.GPIO/

;; For info on /dev/mem (and by extension /dev/gpiomem):
;;
;;   $ man 4 mem
;;
;; https://github.com/raspberrypi/linux/pull/1112
;;
;;   Background for /dev/gpiomem

(library (rpi gpiomem)
  (export
   gpio-open gpio-close
   gpio-function-select
   gpio-high! gpio-low!
   gpio-read

   ;; Low-level procs. Export for debugging.
   reg-read reg-set!)
  (import
   (chezscheme)
   (rpi ftypes-util))

  (define load-libc
    (load-shared-object "libc.so.6"))

  (c_funcs
   ;; int open(const char* pathname, int flags);
   (open	(string int)				int)
   ;; int close(int fd);
   (close	(int)					int)
   ;; void* mmap(void* addr, size_t length, int prot, int flags, int fd, off_t offset);
   ;;   NB: offset should be off_t but Chez ftypes doesn't define it. GLIBC's unistd.h in its description
   ;;   of type sizes groups pointer and off_t together, as at 2019 anyway, so use pointer instead.
   (mmap	(void* size_t int int int ptrdiff_t)	void*)
   ;; int munmap(void* addr, size_t length);
   (munmap	(void* size_t)				int))

  ;; internal: ftype pointer to our mmap'd memory.
  (define gpio-memory #f)

  ;; datasheet pg 90 specifies 41 x 32-bit registers for GPIO.
  (define-ftype register-map (array 41 unsigned-32))

  (define BLOCK_SIZE	(* 4 1024))
  (define PAGE_SIZE	(* 4 1024))

  (define O_RDRW	2)
  (define O_SYNC	#x101000)
  (define O_FLAGS	(bitwise-ior O_RDRW O_SYNC))

  (define PROT_READ	1)
  (define PROT_WRITE	2)
  (define PROT_FLAGS	(bitwise-ior PROT_READ PROT_WRITE))

  (define MAP_SHARED	1)

  ;; NB: This only works on linux kernels that have the Raspbian /dev/gpiomem extension.
  (define gpio-open
    (lambda ()
      (let ([fd (open "/dev/gpiomem" O_FLAGS)])
        (let ([mm (mmap 0 BLOCK_SIZE PROT_FLAGS MAP_SHARED fd 0)])
          (close fd)
          (set! gpio-memory (make-ftype-pointer register-map mm))
          gpio-memory))))

  (define reg-set!
    (lambda (offset value)
      (ftype-set! register-map (offset) gpio-memory value)))

  (define reg-read
    (lambda (offset)
      (ftype-ref register-map (offset) gpio-memory)))

  (define gpio-close
    (lambda ()
      (munmap (ftype-pointer-address gpio-memory) BLOCK_SIZE)))

  ;; GPIO Function Select Registers (GPFSELn)
  ;; See datasheet pg 91+.
  (define GPFSEL0 0)

  ;; FSELn lists the pin config value enumeration.
  (enum FSELn
    (FSEL_MASK		#b111)
    (FSEL_INPUT		#b000)
    (FSEL_OUTPUT	#b001)
    (FSEL_ALTERNATE0	#b100)
    (FSEL_ALTERNATE1	#b101)
    (FSEL_ALTERNATE2	#b110)
    (FSEL_ALTERNATE3	#b111)
    (FSEL_ALTERNATE4	#b011)
    (FSEL_ALTERNATE5	#b010))

  (define FSELn->symbol
    (lambda (x)
      (cond
       [(fx=? x FSEL_INPUT)		'INPUT]
       [(fx=? x FSEL_OUTPUT)		'OUTPUT]
       [(fx=? x FSEL_ALTERNATE0)	'ALTERNATE0]
       [(fx=? x FSEL_ALTERNATE1)	'ALTERNATE1]
       [(fx=? x FSEL_ALTERNATE2)	'ALTERNATE2]
       [(fx=? x FSEL_ALTERNATE3)	'ALTERNATE3]
       [(fx=? x FSEL_ALTERNATE4)	'ALTERNATE4]
       [(fx=? x FSEL_ALTERNATE5)	'ALTERNATE5]
       [else
        #f])))

  (define symbol->FSELn
    (lambda (x)
      (case x
        [(INPUT)	FSEL_INPUT]
        [(OUTPUT)	FSEL_OUTPUT]
        [(ALTERNATE0)	FSEL_ALTERNATE0]
        [(ALTERNATE1)	FSEL_ALTERNATE1]
        [(ALTERNATE2)	FSEL_ALTERNATE2]
        [(ALTERNATE3)	FSEL_ALTERNATE3]
        [(ALTERNATE4)	FSEL_ALTERNATE4]
        [(ALTERNATE5)	FSEL_ALTERNATE5]
        [else
         #f])))

  (define gpfsel-register-and-shift
    (lambda (gpio)
      ;; Each GPFSEL register can configure 10 GPIO pins @ 3 bits per GPIO (see FSELn).
      (let-values ([(register mod) (fxdiv-and-mod gpio 10)])
        (values register (* 3 mod)))))

  (define gpio-function-select
    (case-lambda
     [(gpio)		; get
      (let-values ([(reg off) (gpfsel-register-and-shift gpio)])
        (let ([data (reg-read reg)]
              [mask (bitwise-arithmetic-shift-left FSEL_MASK off)])
          (FSELn->symbol (bitwise-arithmetic-shift-right (bitwise-and data mask) off))))]
     [(gpio function)	; set
      (let-values ([(reg off) (gpfsel-register-and-shift gpio)])
        (let ([data (reg-read reg)]
              [mask (bitwise-not (bitwise-arithmetic-shift-left FSEL_MASK off))]
              [val (symbol->FSELn function)])
          (if val
            (reg-set! reg
             (bitwise-ior (bitwise-and mask data)	; zero the old gpio bits
                          (bitwise-arithmetic-shift-left val off)))
            (display "invalid value\n"))))]))

  ;; GPIO Pin Output Set Registers (GPSETn)
  ;; See datasheet pg 95.
  (define GPSET0 7)

  (define gpset-register-and-shift
    (lambda (gpio)
      (fxdiv-and-mod gpio 32)))

  (define gpio-high!
    (lambda (gpio)
      (let-values ([(reg off) (gpset-register-and-shift gpio)])
        (reg-set! (+ GPSET0 reg) (bitwise-arithmetic-shift-left 1 off)))))

  ;; GPIO Pin Output Clear Registers (GPCLRn)
  ;; See datasheet pg 95-96.
  (define GPCLR0 10)

  (define gpio-low!
    (lambda (gpio)
      (let-values ([(reg off) (gpset-register-and-shift gpio)])
        (reg-set! (+ GPCLR0 reg) (bitwise-arithmetic-shift-left 1 off)))))

  ;; GPIO Pin Level Registers (GPLEVn)
  ;; See datasheet pg 96.
  (define GPLEV0 13)

  (define gpio-read
    (lambda (gpio)
      (let-values ([(reg off) (gpset-register-and-shift gpio)])
        (bitwise-and #x1 (bitwise-arithmetic-shift-right (reg-read (+ GPLEV0 reg)) off))))))
