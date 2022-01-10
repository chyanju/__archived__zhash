#lang rosette
(provide (all-defined-out))

; forewords (some clarifications for reference)
; 1. a term is a symbolic
; 2. an union is a symbolic
; 3. a list of terms is a symbolic (!!), but it's not a term nor a union
; 4. a union of list is a symbolic, a union, but not a term
; 5. a union is NOT a term, vice versa

; for panic use only
(define (println-and-exit msg)
    (printf "~a\n" msg)
    (exit 0)
)

(define zvoid 'zvoid)
(define (zvoid? obj)
    (cond
        [(symbolic? obj) (for/all ([p obj #:exhaustive]) (zvoid? p))]
        [else (equal? zvoid obj)]
    )
)


(struct zhash (key-index-map val-list) #:mutable #:transparent #:reflection-name 'zhash)

; by default, key-index-map is an empty hash, val-list is an empty list
(define (make-zhash) (zhash (make-hash) (list)))
(define (zhash-keys arg-zhash) (zhash-key-index-map arg-zhash))
(define (zhash-vals arg-zhash) (zhash-val-list arg-zhash))

; return a copy of a newly constructed list with element in the designated position replaced by given one
; - arg-ind should be concrete, if not concrete, the caller should wrap a for/all first
; - arg-val can be anything
; - arg-list can be symbolic, but by construction, we don't need for/all here
(define (zhash-val-set arg-zhash arg-ind arg-val)
    (when (or (not (integer? arg-ind)) (symbolic? arg-ind)) (println-and-exit (format "# [zhash-panic] zhash-val-set: arg-ind should be a concrete integer, got: ~a." arg-ind)))
    (let ([arg-val-list (zhash-val-list arg-zhash)])
        (cond
            ; (note) val-list here in rosette can be
            ; - a concrete list
            ; - a list of terms, e.g., (list (ite ...) (ite ...)), whose length is fixed
            ; - (note) by construction, it will NOT be a union
            [(list? arg-val-list)
                ; here the list is either concrete or a list of symbolics
                ; either way the length is fixed
                (let ([n (length arg-val-list)])
                    (when (symbolic? n) (println-and-exit (format "# [zhash-panic] zhash-val-set: length of a list should not be symbolic, got: ~a." n)))
                    (for/list ([i (range n)]) (if (equal? arg-ind i) arg-val (list-ref arg-val-list i)))
                )
            ]
            [else (println-and-exit (format "# [zhash-panic] zhash-val-set: unsupported list type, got: ~a." arg-val-list))]
        )
    )
)
(define (zhash-val-set! arg-zhash arg-ind arg-val)
    (set-zhash-val-list! arg-zhash (zhash-val-set arg-zhash arg-ind arg-val))
)

; add an element to the end of a val-list and return a copy of the new list
; see val-list-set for requirement details
(define (zhash-val-append arg-zhash arg-val)
    (let ([arg-val-list (zhash-val-list arg-zhash)])
        (cond
            [(list? arg-val-list) (reverse (cons arg-val (reverse arg-val-list)))]
            [else (println-and-exit (format "# [zhash-panic] zhash-val-append: unsupported list type, got: ~a." arg-val-list))]
        )
    )
)
(define (zhash-val-append! arg-zhash arg-val)
    (set-zhash-val-list! arg-zhash (zhash-val-append arg-zhash arg-val))
)

; check the key-index-map for existence of a key
; - arg-key should be concrete, if not concrete, the caller should wrap a for/all first
(define (zhash-key-exists? arg-zhash arg-key)
    (when (symbolic? arg-key) (println-and-exit (format "# [zhash-panic] zhash-key-exists?: arg-key should be concrete, got: ~a." arg-key)))
    (let ([arg-kim (zhash-key-index-map arg-zhash)])
        (hash-has-key? arg-kim arg-key)
    )
)

; check the **factual** existence of a key
; if the corresponding value of this key is zvoid, it means the key is **factually** non-existent
(define (zhash-has-key? arg-zhash arg-key)
    (cond
        [(symbolic? arg-key) (for/all ([dkey arg-key #:exhaustive]) (zhash-has-key? arg-zhash dkey))]
        [else
            (if (zhash-key-exists? arg-zhash arg-key)
                ; key exists in the key-index-map, check whether it's zvoid
                (let ([val-list (zhash-val-list arg-zhash)]
                      [key-index-map (zhash-key-index-map arg-zhash)])
                    (if (zvoid? (list-ref val-list (hash-ref key-index-map arg-key)))
                        #f ; zvoid will be treated as key doesn't exist
                        #t ; otherwise the key exists
                    )
                )
                ; key doesn't exist in the key-index-map, directly return #f
                #f
            )
        ]
    )
)

; make sure arg-key exists in key-index-map
; if not, add it
(define (zhash-secure-key! arg-zhash arg-key)
    (printf "# securing key: ~a\n" arg-key)
    (cond
        [(symbolic? arg-key) (for/all ([dkey arg-key #:exhaustive]) (zhash-secure-key! arg-zhash dkey))]
        [else
            (when (not (zhash-key-exists? arg-zhash arg-key))
                (let ([key-index-map (zhash-key-index-map arg-zhash)]
                       [n (length (zhash-val-list arg-zhash))])
                    (when (symbolic? n) (println-and-exit (format "# [zhash-panic] zhash-secure-key!: length of val-list should not be symbolic, got: ~a." n)))
                    ; (fixme) you probably want to temporarily clear the vc here
                    (hash-set! key-index-map arg-key n) ; add the key to the key-index-map
                    (zhash-val-append! arg-zhash zvoid) ; add zvoid to the val-list, secure a position
                )
            )
        ]
    )
)

; (note) if the key doesn't exist, the path will authmatically be cut by rosette, which is expected
(define (zhash-ref arg-zhash arg-key)
    (cond
        [(symbolic? arg-key) (for/all ([dkey arg-key #:exhaustive]) (zhash-ref arg-zhash dkey))]
        [else 
            (let ([val-list (zhash-val-list arg-zhash)]
                  [key-index-map (zhash-key-index-map arg-zhash)])
                (list-ref val-list (hash-ref key-index-map arg-key))
            )
        ]
    )
)

; (note) if the key doesn't exist, the path will authmatically be cut by rosette, which is expected
; this returns a copy of a newly set val-list
(define (zhash-set! arg-zhash arg-key arg-val)
    ; first secure all the keys
    ; this will update val-list to make sure of sufficient slots before actual filling of values
    (zhash-secure-key! arg-zhash arg-key)
    ; then set the value
    (define (exhaustive-set! ex-key)
        (cond
            [(symbolic? ex-key) (for/all ([dkey ex-key #:exhaustive]) (exhaustive-set! dkey))]
            [else
                (let* ([key-index-map (zhash-key-index-map arg-zhash)]
                       [ind (hash-ref key-index-map ex-key)])
                    (zhash-val-set! arg-zhash ind arg-val)
                )
            ]
        )
    )
    (exhaustive-set! arg-key)
)

(define (zhash-regularize-vals! arg-zhash)
    (let ([val-list (zhash-val-list arg-zhash)])
        (cond
            [(union? val-list)
                ; first find out the branch with longest val-list length
                (define max-length (apply max (for/list ([vv (union-contents val-list)]) (length vv))))
                ; then for every branch extend the val-list to max-length
                ; (fixme) carefully think about the correctness of this
                (set-zhash-val-list!
                    arg-zhash
                    (for/all ([vv val-list #:exhaustive])
                        (let ([curr-length (length vv)])
                            (append 
                                vv
                                (for/list ([_ (range (- max-length curr-length))]) zvoid)
                            )
                        )
                    )
                )
            ]
            ; else do nothing
        )
    )
)














