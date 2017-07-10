#lang racket

; MD5 implementation
;;based on rfc1321, wikipedia article, and https://raw.githubusercontent.com/CastixGitHub/racket-md5/master/md5.rkt
;;this is for owner learning only

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

(define (digest msg)
  (define padded-msg (add-padding-bit msg))
  (define-values (a0 b0 c0 d0)
    (for/fold ([a0 #x67452301] [b0 #xefcdab89] [c0 #x98badcfe] [d0 #x10325476])
              ([chunk (bytes-split padded-msg 64)])
      (define M (for/list ([x (bytes-split chunk 4)])
                  (integer-bytes->integer x #f #f)))
      (define-values (A B C D)
        (for/fold ([A a0] [B b0] [C c0] [D d0])
                  ([i (in-range 64)])
          (define-values (F1 g)
            (cond
              [(<= 0 i 15) (values (F B C D) i)]
              [(<= 16 i 31) (values (G B C D) (modulo (add1 (* 5 i)) 16))]
              [(<= 32 i 47) (values (H B C D) (modulo (word+ (* 3 i) 5) 16))]
              [(<= 48 i 63) (values (I B C D) (modulo (* 7 i) 16))]))
          (define dTemp D)
          (define bTemp (word+ B (leftrotate (+ A F1 (list-ref K i) (list-ref M g)) (list-ref s i)))) 
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

;helper function
;;used to split the message into 64 bytes chunks
;;and to split each chunk into sixteen 4 bytes word
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

;Unit test
(module+ test ;; add submodule test
  (require rackunit)
  (check-equal? (digest #"Runs all of the tests specified by check forms in the current module and reports the") "21c0b2cbeee1c110d97c32925a469eeb")
  (check-not-equal? (digest #"Runs all of the tests specified by check forms in the current module and reports the ") "21c0b2cbeee1c110d97c32925a469eeb")

  ;Test
  (require test-engine/racket-tests)
  (check-expect (digest #"a") "0cc175b9c0f1b6a831c399e269772661")
  (check-expect (digest #"Runs all of the tests specified by check forms in the current module and reports the") "21c0b2cbeee1c110d97c32925a469eeb")
  ;;run test
  (test)
)
