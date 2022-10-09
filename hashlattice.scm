(use-modules (ice-9 sandbox))
(use-modules (gcrypt hash))

(define hash-bytes sha256)

(define (bv->hex bv)
  (apply string-append (map (lambda (b) (format #f "~2,'0x" b)) (bytevector->u8-list bv))))

(define (hex->bv str)
  (when (= 1 (modulo (string-length str) 2))
    (error "hex->bv: odd number of hex digits"))
  (u8-list->bytevector
    (map (lambda (i) (string->number (string-copy str i (+ 2 i)) 16))
         (iota (/ (string-length str) 2) 0 2))))


; get-dep : key graphsym -> elem
; no graphhash -> get from the current graph
; no mutual recursion

(make-record-type "latgraph"
  '(dep-graphs  ; alist symbol
    is-key?     ; key -> bool
    key-<       ; key key -> bool
    is-elem?    ; key elem get-dep -> bool
    bottom      ; key get-dep -> elem
    join        ; key elem elem get-dep -> elem
    transport)) ; key elem get-dep1 get-dep2 -> elem

; mapping : key -> elem

; we can load latgraphs by their hash codes

(define time-limit 20) ; seconds
(define alloc-limit 1000000000) ; bytes

(define (assv-value key alist)
  (let ((pair (assv key alist)))
    (if pair
      (cdr pair)
      (error "assv-value: key not found" key alist))))

(define (check-function f)
  (if (procedure? f)
    f
    (error "check-function: not a procedure" f)))

(define (load-latgraph graphexp)
  (let ((module (eval-in-sandbox graphexp #:time-limit time-limit #:allocaction-limit alloc-limit)))
    (make-latgraph
      (assv-value 'dep-graphs module)
      (check-function (assv-value 'is-key? module))
      (check-function (assv-value 'key-< module))
      (check-function (assv-value 'is-elem? module))
      (check-function (assv-value 'bottom module))
      (check-function (assv-value 'join module))
      (check-function (assv-value 'transport module)))))
