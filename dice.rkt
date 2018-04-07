#lang racket
(require 2htdp/image)
(require lang/posn)
(require (only-in threading ~>)) ; "threading-lib" package

(define colors
  (list "red" "orange" "yellow" "green" "cyan" "blue" "purple"))

; rainbow colors
(apply beside
       (map (lambda (color) (rectangle 20 20 "solid" color)) colors))

(struct pt (x y z) #:transparent)

; x軸周りの回転
(define (rotate-x rad p)
  (let ([s (sin rad)] [c (cos rad)] [y (pt-y p)] [z (pt-z p)])
    (pt (pt-x p) (- (* c y) (* s z)) (+ (* s y) (* c z)))))

; y軸周りの回転
(define (rotate-y rad p)
  (let ([s (sin rad)] [c (cos rad)] [x (pt-x p)] [z (pt-z p)])
    (pt (+ (* s z) (* c x)) (pt-y p) (- (* c z) (* s x)))))

; z軸周りの回転
(define (rotate-z rad p)
  (let ([s (sin rad)] [c (cos rad)] [x (pt-x p)] [y (pt-y p)])
    (pt (- (* c x) (* s y)) (+ (* s x) (* c y)) (pt-z p))))

; 拡大
(define (scalar-mult mag p)
  (pt (* mag (pt-x p)) (* mag (pt-y p)) (* mag (pt-z p))))

; 平行移動
(define (shift x y z p)
  (pt (+ x (pt-x p)) (+ y (pt-y p)) (+ z (pt-z p))))

; 内積
(define (get-inner-product p1 p2)
  (+ (* (pt-x p1) (pt-x p2))
     (* (pt-y p1) (pt-y p2))
     (* (pt-z p1) (pt-z p2))))

; サイコロ
; - 雌サイコロ：天一地六東五西二南三北四
;           | z               1 4
;           |/                |/
;         --*-x             2-*-5
;          /|                /|
;         / y               3 6
; [coordinate system] [number of dice]

(define standard-dice-plane-center
  (hash 1 (pt 0 -1 0) 2 (pt -1 0 0) 3 (pt 0 0 -1)
        4 (pt 0 0 1) 5 (pt 1 0 0) 6 (pt 0 1 0)))

(define standard-dice-points
  (let ((points (make-vector 8 0)))
    (define (p pos pt) (vector-set! points pos pt))
    (p 0 (pt -1 -1 1)) (p 1 (pt -1 -1 -1))
    (p 2 (pt 1 -1 -1)) (p 3 (pt 1 -1 1))
    (p 4 (pt -1 1 1)) (p 5 (pt -1 1 -1))
    (p 6 (pt 1 1 -1)) (p 7 (pt 1 1 1))
    points))

(define dice-frame-point-numbers
  #hash( (1 . #(0 1 2 3)) (2 . #(0 1 5 4)) (3 . #(1 2 6 5))
         (4 . #(3 0 4 7)) (5 . #(2 3 7 6)) (6 . #(4 5 6 7))))

(define linked-point-pairs
  '( (0 1) (1 2) (2 3) (3 0)
     (0 4) (1 5) (2 6) (3 7)
     (4 5) (5 6) (6 7) (7 4) ) )

(define (planes->linked-point-pairs planes)
  (let ([s (mutable-set)])
    (for* ([i planes]
           [j (hash-ref dice-frame-point-numbers i)])
      (set-add! s j))
    (filter (lambda (pair) (and (set-member? s (first pair))
                                (set-member? s (second pair))))
            linked-point-pairs)))

(define (map/dice proc h)
  (for/hash ([i (in-range 1 7)])
    (values i (proc (hash-ref h i)))))

(define dice-front (pt 0 0 -1))

(define dice%
  (class object%
    (super-new)
    (init [size 10] [center-x 20] [center-y 20] [center-z 20]
          [rad-x 0] [rad-y 0] [rad-z 0])
    (define *size* size)
    (define *center-x* center-x)
    (define *center-y* center-y)
    (define *center-z* center-z)
    (define *rad-x* rad-x)
    (define *rad-y* rad-y)
    (define *rad-z* rad-z)

    (define *pts*
      (for/vector ([p standard-dice-points])
        (~> p
            (rotate-x *rad-x* _)
            (rotate-y *rad-y* _)
            (rotate-z *rad-z* _)
            (scalar-mult *size* _)
            (shift *center-x* *center-y* *center-z* _))))

    (define/public (print-points)
      (for ([i (in-range 0 8 2)])
        (printf "~a: ~a ~a: ~a~n"
                i
                (vector-ref *pts* i)
                (add1 i)
                (vector-ref *pts* (add1 i)))))
    
    (define/public (draw-frame-model)
      (define (x i) (pt-x (vector-ref *pts* i)))
      (define (y i) (pt-y (vector-ref *pts* i)))
      (~> empty-image
          (add-line _ (x 0) (y 0) (x 1) (y 1) "black")
          (add-line _ (x 1) (y 1) (x 2) (y 2) "black")
          (add-line _ (x 2) (y 2) (x 3) (y 3) "black")
          (add-line _ (x 3) (y 3) (x 0) (y 0) "black")
          (add-line _ (x 4) (y 4) (x 5) (y 5) "black")
          (add-line _ (x 5) (y 5) (x 6) (y 6) "black")
          (add-line _ (x 6) (y 6) (x 7) (y 7) "black")
          (add-line _ (x 7) (y 7) (x 4) (y 4) "black")
          (add-line _ (x 0) (y 0) (x 4) (y 4) "black")
          (add-line _ (x 1) (y 1) (x 5) (y 5) "black")
          (add-line _ (x 2) (y 2) (x 6) (y 6) "black")
          (add-line _ (x 3) (y 3) (x 7) (y 7) "black")))

    (define *dir*
      (map/dice (lambda (p) (~> p
                                (rotate-x rad-x _)
                                (rotate-y rad-y _)
                                (rotate-z rad-z _)))
                standard-dice-plane-center))

    (define *front-planes*
      (filter (lambda (i) (> (get-inner-product (hash-ref *dir* i)
                                                dice-front)
                             0))
              (range 1 7)))

    (define/public (get-front-planes)
      *front-planes*)

    (define *front-line-end-point-numbers*
      (planes->linked-point-pairs *front-planes*))

    (define/public (get-front-line-end-point-numbers)
      *front-line-end-point-numbers*)

    (define/public (draw-colored-model)

      (define (get-2d-position point-index)
        (let ([p (vector-ref *pts* point-index)])
          (make-posn (pt-x p) (pt-y p))))
      
      (define (get-polygon-adder plane-index)
        (let ([pt-nums (hash-ref dice-frame-point-numbers plane-index)]
              [color (list-ref colors (sub1 plane-index))])
          (lambda (image)
            (add-polygon image
                         (map get-2d-position (vector->list pt-nums))
                         "solid"
                         color))))

      (foldl (lambda (i image) ((get-polygon-adder i) image))
             empty-image
             *front-planes*))
))
