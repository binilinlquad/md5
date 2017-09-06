#lang racket

; MD5 implementation
;;It is buggy when generate md5 for long text
;;based on rfc1321, wikipedia article, and https://raw.githubusercontent.com/CastixGitHub/racket-md5/master/md5.rkt
;;this is for owner learning only

(define s (list 7 12 17 22  7 12 17 22  7 12 17 22  7 12 17 22
                5  9 14 20  5  9 14 20  5  9 14 20  5  9 14 20
                4 11 16 23  4 11 16 23  4 11 16 23  4 11 16 23
                6 10 15 21  6 10 15 21  6 10 15 21  6 10 15 21))

(define K (for/list ([i (in-range 64)])
    (inexact->exact (floor (* (expt 2 32) (abs (sin (add1 i))))))))

;;F(X,Y,Z) = XY v not(X) Z
(define (F x y z) (bitwise-ior (bitwise-and x y) (bitwise-and (bitwise-not x) z)))
;G(X,Y,Z) = XZ v Y not(Z)
(define (G x y z) (bitwise-ior (bitwise-and x z) (bitwise-and y (bitwise-not z))))
;H(X,Y,Z) = X xor Y xor Z
(define (H x y z) (bitwise-xor x y z))
;I(X,Y,Z) = Y xor (X v not(Z))
(define (I x y z) (bitwise-xor y (bitwise-ior x (bitwise-not z))))

;; 64 bytes = 512 bits
;; 56 bytes = 448 bits
(define (add-padding-zero msg)
  (define len (bytes-length msg))
  (if (equal? (modulo len 64) 56)
      msg
      (add-padding-zero (bytes-append msg (bytes 0)))))

(define (add-padding-bit msg)
  (define (bit-length msg) (* (bytes-length msg) 8))
  (define length (word-limit (bit-length msg)))
  (define padded-msg (bytes-append msg (bytes #b10000000)))
  (bytes-append
   (add-padding-zero padded-msg)
   (integer->integer-bytes length 8 #f #f)))

; calculate length after message padding
(define (calculate original-length)
   (define length-after-1byte (add1 original-length))
   (define length-zeroes (- 56 (modulo length-after-1byte 64)))
   (+ length-after-1byte length-zeroes 8))

;; generator produce stream
;; in-generator is used to generator as sequence
(require racket/generator)

;; Prepare String as input port
(define (prepare-msg message block-size)
  (define msg-as-bytes (cond [(string? message) (string->bytes/utf-8 message)]
                             [(bytes? message) message]))
  (define msg-bytes-length (bytes-length msg-as-bytes))
  (define msg-bits-length (* msg-bytes-length 8))
  (define pad-zero (make-bytes (- 56 (modulo (add1 msg-bytes-length) 64)) 0))
  (let ([msg (open-input-bytes msg-as-bytes)]
        [bit1 (open-input-bytes (bytes #b10000000))]
        [bit0 (open-input-bytes pad-zero)]
        [length (open-input-bytes (integer->integer-bytes (word-limit msg-bits-length) 8 #f #f))])
    (define port (input-port-append #t msg bit1 bit0 length))
    ; body
    (in-generator 
     (let loop ([blocks (read-bytes block-size port)])
       (when (not (eof-object? blocks))
         (yield blocks)
         (loop (read-bytes block-size port))
         )))))

;; Prepare File as input port
(define (prepare-file filename block-size)
  (define msg-bytes-length (file-size filename))
  (define msg-bits-length (* msg-bytes-length 8))
  (define pad-zero (make-bytes (- 56 (modulo (add1 msg-bytes-length) 64)) 0))
  (let ([msg (open-input-file filename)]
        [bit1 (open-input-bytes (bytes #b10000000))]
        [bit0 (open-input-bytes pad-zero)]
        [length (open-input-bytes (integer->integer-bytes (word-limit msg-bits-length) 8 #f #f))])
    (define port (input-port-append #t msg bit1 bit0 length))
    ; body
    (in-generator 
     (let loop ([blocks (read-bytes block-size port)])
       (when (not (eof-object? blocks))
         (yield blocks)
         (loop (read-bytes block-size port))
         )))))

;; Main Function
(define (digest preprocessor msg)
  (define block-size 64)
  (define (block->list/integer block)
    (for/list ([x (bytes-split block 4)])
      (integer-bytes->integer x #f #f)))

  (define-values (a0 b0 c0 d0) 
    (for/fold ([a0 #x67452301] [b0 #xefcdab89] [c0 #x98badcfe] [d0 #x10325476])
              ([chunk-bytes (preprocessor msg block-size)])
      (define chunk (block->list/integer chunk-bytes))
      (define-values (A B C D)
        (for/fold ([A a0] [B b0] [C c0] [D d0])
                  ([i (in-range block-size)])
          (define-values (F1 g)
            (cond
              [(<= 0 i 15) (values (F B C D) i)]
              [(<= 16 i 31) (values (G B C D) (modulo (add1 (* 5 i)) 16))]
              [(<= 32 i 47) (values (H B C D) (modulo (word+ (* 3 i) 5) 16))]
              [(<= 48 i 63) (values (I B C D) (modulo (* 7 i) 16))]))
          (define dTemp D)
          (define bTemp (word+ B (leftrotate (+ A F1 (list-ref K i) (list-ref chunk g)) (list-ref s i)))) ; M only be used here
          (values dTemp bTemp B C)))
    (values (word+ a0 A) (word+ b0 B) (word+ c0 C) (word+ d0 D))))
  (appendAll a0 b0 c0 d0))

(define (appendAll a b c d)
  (define all-bytes
  (bytes-append
  (integer->integer-bytes a 4 #f #f)
  (integer->integer-bytes b 4 #f #f)
  (integer->integer-bytes c 4 #f #f)
  (integer->integer-bytes d 4 #f #f)
  ))
  (apply string-append
         (for/list ([b all-bytes])
           (~a #:width 2 #:pad-string "0" #:align 'right
               (number->string b 16)))))

;; Helper
;; used to split the message into 64 bytes chunks
;; and to split each chunk into sixteen 4 bytes word
(define (bytes-split bytes size)
  (define len (bytes-length bytes))
  (unless (= 0 (modulo len size))
    ((raise-type-error
     'split-bytes "byte string of length divisible by size" 0 bytes)))
  (for/list ([i (in-range (/ len size))])
    (subbytes bytes (* size i) (* size (add1 i)))))

(define (word-limit x) (bitwise-and #xffffffff x))
(define (leftrotate x c)
  ;https://en.wikipedia.org/wiki/Circular_shift
  (set! x (word-limit x))
  (word-limit (bitwise-ior
               (arithmetic-shift x c)
               (arithmetic-shift x (* -1 (- 32 c))))))
(define (word+ w0 w1)
  (word-limit (+ w0 w1)))

(define (split message)(bytes-split message 64))

;; Shortcut
(define (digest-bytes bytes) (digest prepare-msg bytes))
(define (digest-string str) (digest prepare-msg str))

(require 2htdp/batch-io)
(define (digest-file path) (digest prepare-file path))

;; test
(module+ test ;; add submodule test
  (require rackunit)
  (check-equal? (digest-bytes #"Runs all of the tests specified by check forms in the current module and reports the") "21c0b2cbeee1c110d97c32925a469eeb")
  (check-not-equal? (digest-bytes #"Runs all of the tests specified by check forms in the current module and reports the ") "21c0b2cbeee1c110d97c32925a469eeb")

  ;Test
  (require test-engine/racket-tests)
  (check-expect (calculate (bytes-length #"Runs all of the tests specified by check forms in the current module and reports the"))
                (bytes-length (add-padding-bit #"Runs all of the tests specified by check forms in the current module and reports the")))
  
  (check-expect (digest-bytes #"a") "0cc175b9c0f1b6a831c399e269772661")
  (check-expect (digest-bytes #"Runs all of the tests specified by check forms in the current module and reports the") "21c0b2cbeee1c110d97c32925a469eeb")
  (check-expect (digest-string "a") "0cc175b9c0f1b6a831c399e269772661")
  (check-expect (digest-file "sicp.pdf") "3bed4a05ae7fc66cd90c2e292c70d2b4")
  ;;run test
  (test)
)
