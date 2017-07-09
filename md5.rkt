#lang racket

; MD5 implementation
;;based on rfc1321, wikipedia article, and https://raw.githubusercontent.com/CastixGitHub/racket-md5/master/md5.rkt
;;this is for owner learning only
(define max-int32 #xffffffff)

(define (bit-length msg)
  (* (bytes-length msg) 8))

(define (original-length msg)
  (bitwise-and max-int32 (bit-length msg)))

(define (add-bit-1-and-7-zero msg)
  (bytes-append msg (bytes #b10000000)))

;; 64 bytes = 512 bits
;; 56 bytes = 448 bits
(define (add-padding-zero msg)
  (define len (bytes-length msg))
  (if (equal? (modulo len 64) 56)
      msg
      (add-padding-zero (bytes-append msg (bytes 0)))))

(define (add-padding-bit msg)
  (bytes-append (add-padding-zero (add-bit-1-and-7-zero msg)) (integer->integer-bytes (original-length msg) 8 #f #f)))

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
  (define a0 #x67452301)
  (define b0 #xefcdab89)
  (define c0 #x98badcfe)
  (define d0 #x10325476)

  (for/list ([chunk (bytes-split padded-msg 64)])
    (define A a0)
    (define B b0)
    (define C c0)
    (define D d0)
    (define M (for/list ([x (bytes-split chunk 4)])
                (integer-bytes->integer x #f #f)))
    (define F1 0)
    (define g 0)

    ;(display M)
    (for/list ([i (in-range 64)])
      (cond
        [(<= 0 i 15)
         (set! F1 (F B C D))
         (set! g i)
        ]
        [(<= 16 i 31)
         (set! F1 (G B C D))
         (set! g (modulo (add1 (* 5 i)) 16))
        ]
        [(<= 32 i 47)
         (set! F1 (H B C D))
         (set! g (modulo (word+ (* 3 i) 5) 16))
        ]
        [(<= 48 i 63)
         (set! F1 (I B C D))
         (set! g (modulo (* 7 i) 16))
        ]
        )
      (define dTemp D)
      (set! D C)
      (set! C B)
      (set! B (word+ B (leftrotate (+ A F1 (list-ref K i) (list-ref M g)) (list-ref s i))))
      (set! A dTemp)
    )
    (set! a0 (word+ a0 A))
    (set! b0 (word+ b0 B))
    (set! c0 (word+ c0 C))
    (set! d0 (word+ d0 D))
  )
  (sumAll a0 b0 c0 d0)
  
)

(define (sumAll a b c d)
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

(define (word-limit x) (bitwise-and max-int32 x))
(define (leftrotate x c)
  ;https://en.wikipedia.org/wiki/Circular_shift
  (set! x (word-limit x))
  (word-limit (bitwise-ior
               (arithmetic-shift x c)
               (arithmetic-shift x (* -1 (- 32 c))))))
(define (word+ w0 w1)
  (word-limit (+ w0 w1)))

(define (split message)(bytes-split message 64))
