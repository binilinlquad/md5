#lang racket

;;;; MD5 implementation
;;;; It is buggy when generate md5 for long text
;;;; based on rfc1321, wikipedia article, and https://raw.githubusercontent.com/CastixGitHub/racket-md5/master/md5.rkt
;;;; this is for owner learning only

;make these functions public
(provide digest-bytes digest-string digest-file)

;;;; Code:
(define s
  (list 7 12 17 22  7 12 17 22  7 12 17 22  7 12 17 22
        5  9 14 20  5  9 14 20  5  9 14 20  5  9 14 20
        4 11 16 23  4 11 16 23  4 11 16 23  4 11 16 23
        6 10 15 21  6 10 15 21  6 10 15 21  6 10 15 21))

(define K
  (for/list ([i (in-range 64)])
    (inexact->exact (floor (* (expt 2 32) (abs (sin (add1 i))))))))

;F(X,Y,Z) = XY v not(X) Z
(define (F x y z)
  (bitwise-ior (bitwise-and x y)
               (bitwise-and (bitwise-not x) z)))

;G(X,Y,Z) = XZ v Y not(Z)
(define (G x y z)
  (bitwise-ior (bitwise-and x z)
               (bitwise-and y (bitwise-not z))))

;H(X,Y,Z) = X xor Y xor Z
(define (H x y z)
  (bitwise-xor x y z))

;I(X,Y,Z) = Y xor (X v not(Z))
(define (I x y z)
  (bitwise-xor y (bitwise-ior x (bitwise-not z))))

;; generator produce stream
;; in-generator is used to generator as sequence
(require racket/generator)

;; 64 bytes = 512 bits
;; 56 bytes = 448 bits
(define block-size 64)

;; Main Function
(define (digest msg-port)
  ;; convert bytes into integers
  (define (block->list/integer block)
    (for/list ([x (bytes-split block 4)])
      (integer-bytes->integer x #f #f)))
  ;; Main Algorithm
  (define-values (a0 b0 c0 d0)
    (for/fold ([a0 #x67452301] [b0 #xefcdab89] [c0 #x98badcfe] [d0 #x10325476])
              ([chunk-bytes msg-port])
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
  ; concat result as string
  (appendAll a0 b0 c0 d0))

(define (convert-to-bytes a b c d)
  ; Convert passed integer of a, b, c, d into one bytes
  (apply bytes-append
         (map (lambda (number)
                (integer->integer-bytes number 4 #f #f))
              (list a b c d))))

(define (appendAll a b c d)
  ; append all passed integers to a string
  (let ([all-bytes (convert-to-bytes a b c d)])
    (apply string-append
           (for/list ([b all-bytes])
             (~a #:width 2 #:pad-string "0" #:align 'right
                 (number->string b 16))))))


(define (bytes-split bytes size)
  ; split message into 64 bytes chunks. Each chunk then splitted again into 16 of 4 bytes word
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

(define (message->bytes message)
  ; convert message, i.e string or bytes format to bytes
  (cond [(string? message) (string->bytes/utf-8 message)]
        [(bytes? message) message]))

(define (pad-zero-amount length-in-bytes)
  (- 56 (modulo (add1 length-in-bytes) 64)))
  
(define (prepare-message text file?)
  ; Prepare provided text as input port. Text could be string, bytes, or filepath
  (define-values (msg-input-port msg-bytes-length)
    (if file?
        (values (open-input-file text) (file-size text))
        (let ([msg-bytes (message->bytes text)])
          (values (open-input-bytes msg-bytes) (bytes-length msg-bytes)))))
  (create-prepared-port msg-input-port msg-bytes-length))

(define (create-prepared-port msg-input-port msg-bytes-length)
  ; Actual creation input-port for processing
  (let* ([msg-bits-length (* msg-bytes-length 8)]
         [pad-zero (make-bytes (pad-zero-amount msg-bytes-length) 0)]
         [bit1 (open-input-bytes (bytes #b10000000))]
         [bit0 (open-input-bytes pad-zero)]
         [length (open-input-bytes (integer->integer-bytes msg-bits-length 8 #f #f))]
         [port (input-port-append #t msg-input-port bit1 bit0 length)])
    ; body
    (generate-block-sequence port block-size)))

(define (generate-block-sequence port block-size)
  ; Generate prepared message as sequence
  (in-generator (let loop ([blocks (read-bytes block-size port)])
                  (when (not (eof-object? blocks))
                    (yield blocks)
                    (loop (read-bytes block-size port))))))


;; Shortcut
(define (digest-bytes bytes) (digest (prepare-message bytes #f)))
(define (digest-string str) (digest (prepare-message str #f)))

(require 2htdp/batch-io)
(define (digest-file path) (digest (prepare-message path #t)))

;; test
(module+ test ;; add submodule test
  (require rackunit)
  (check-equal? (digest-bytes #"Runs all of the tests specified by check forms in the current module and reports the") "21c0b2cbeee1c110d97c32925a469eeb")
  (check-not-equal? (digest-bytes #"Runs all of the tests specified by check forms in the current module and reports the ") "21c0b2cbeee1c110d97c32925a469eeb")

  ;Test
  (require test-engine/racket-tests)
  (check-expect (digest-bytes #"a") "0cc175b9c0f1b6a831c399e269772661")
  (check-expect (digest-bytes #"Runs all of the tests specified by check forms in the current module and reports the") "21c0b2cbeee1c110d97c32925a469eeb")
  (check-expect (digest-string "a") "0cc175b9c0f1b6a831c399e269772661")
  (check-expect (digest-file "sicp.pdf") "3bed4a05ae7fc66cd90c2e292c70d2b4")

  ;;run test
  (test)
  )
