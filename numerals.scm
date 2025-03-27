(define (repeated F n X)
  (if (= n 0) X
      (F (repeated F (- n 1) X))))

(define (c n)
  (lambda (f)
    (lambda (x)
      (repeated f n x))))

(define 1+ (lambda (x) (+ x 1)))

(define (to-n c)
  ((c 1+) 0))

(define c0 (c 0))
(define c1 (c 1))
(define c5 (c 5))

(define cs
  (lambda (n)
    (lambda (f)
      (lambda (x)
        (f ((n f) x))))))

(define css
  (lambda (n)
    (lambda (f)
      (lambda (x)
        ((n f) (f x))))))

(define c+
  (lambda (m)
    (lambda (n)
      (lambda (f)
        (lambda (x)
          ((m f) ((n f) x)))))))

(define c++
  (lambda (m)
    (lambda (n)
      (lambda (f)
        (lambda (x)
          ((n f) ((m f) x)))))))

(define c#
  (lambda (m) (m cs)))

(define c**
  (lambda (m)
    (lambda (n)
      ((m (c+ n)) c0))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define c*
  (lambda (m)
    (lambda (n)
      (lambda (f)
        (m (n f))))))

(define c^^
  (lambda (m)
    (lambda (n)
      ((n (c* m)) c1))))

(define c^
  (lambda (m)
    (lambda (n)
      (n m))))

(define c#t (lambda (x) (lambda (y) x)))
(define c#f (lambda (x) (lambda (y) y)))

(define (to-b b)
  ((b #t) #f))

(define cnegneg
  (lambda (b) ((b c#f) c#t)))

(define cneg
  (lambda (b)
    (lambda (x)
      (lambda (y)
        ((b y) x)))))

(define cand
  (lambda (a)
    (lambda (b)
      ((a b) c#f))))

(define cor
  (lambda (a) (a c#t)))

(define c=0
  (lambda (n)
    ((n (lambda (m) c#f)) c#t)))

(define ceven
  (lambda (n)
    ((n cneg) c#t)))

(define ccons
  (lambda (x)
    (lambda (y)
      (lambda (z)
        ((z x) y)))))

(define ccar (lambda (p) (p c#t)))
(define ccdr (lambda (p) (p c#f)))

(define (to-p p)
  (cons (ccar p) (ccdr p)))

(define (to-pn pn)
  (cons (to-n (ccar pn)) (to-n (ccdr pn))))

(define cp
  (lambda (n)
    (ccdr ((n
            (lambda (p) ((ccons (cs (ccar p)))
                         (ccar p))))
           ((ccons c0) c0)))))


(define step!
  (lambda (p) ((ccons (cs (ccar p)))
               ((c* (cs (ccar p)))
                (ccdr p)))))

(define c!
  (lambda (n) (ccdr ((n step!) ((ccons c0) c1)))))

(define gamma!
  (lambda (f)
    (lambda (n)
      (((c=0 n) c1)
       ((c* n) (f (cp n)))))))

(define gamma!!
  (lambda (f)
    (lambda (n)
      (((c=0 n) (lambda (y) (c1 y)))
       (lambda (y) (((c* n) (f (cp n))) y))))))


(define Y
  (lambda (f)
    ((lambda (x) (f (x x))) (lambda (x) (f (x x))))))

(define Z
  (lambda (f)
    ((lambda (x) (f (lambda (z) ((x x) z))))
     (lambda (x) (f (lambda (z) ((x x) z)))))))

;; (define c!! (Y gamma!))
;; (define c!! (Z gamma!))
(define c!! (Z gamma!!))
