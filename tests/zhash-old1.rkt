#lang rosette
(provide zvoid zvoid?)
(provide make-zhash zhash-has-key? zhash-ref zhash-set!)
(provide test%)

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

(define (make-zhash) (new zhash%))
(define (zhash-has-key? arg-zhash arg-key) (send arg-zhash zhash-has-key? arg-key))
(define (zhash-ref arg-zhash arg-key) (send arg-zhash zhash-ref arg-key))
(define (zhash-set! arg-zhash arg-key arg-val) (send arg-zhash zhash-set! arg-key arg-val))

(define test%
    (class object%
        (super-new)
        (field
            [test-value 0]
        )
        (define/public (change0)
            (set! test-value 29)
        )
        (define/public (change1)
            (set! test-value 199)
        )
    )
)

(define zhash%
    (class object%
        (super-new)
        (field
            ; this is for indexing, no vc is attached
            ; key if exists -> index
            ; keys with different vcs share the same index
            [key-index-map (make-hash)]

            [val-list (list)]
        )

        ; return a copy of a newly constructed list with element in the designated position replaced by given one
        ; - arg-ind should be concrete, if not concrete, the caller should wrap a for/all first
        ; - arg-val can be anything
        ; - arg-list can be symbolic, but by construction, we don't need for/all here
        ; (define (val-list-set arg-list arg-ind arg-val)
        ;     (when (or (not (integer? arg-ind)) (symbolic? arg-ind)) (println-and-exit (format "# [zhash-panic] val-list-set: arg-ind should be a concrete integer, got: ~a." arg-ind)))
        ;     (cond
        ;         ; (note) a list here in rosette can be
        ;         ; - a concrete list
        ;         ; - a list of terms, e.g., (list (ite ...) (ite ...)), whose length is fixed
        ;         ; - (note) by construction, a list here will NOT be a union
        ;         [(list? arg-list)
        ;             ; here the list is either concrete or a list of symbolics
        ;             ; either way the length is fixed
        ;             (let ([n (length arg-list)])
        ;                 (when (symbolic? n) (println-and-exit (format "# [zhash-panic] val-list-set: length of a list should not be symbolic, got: ~a." n)))
        ;                 (for/list ([i (range n)]) (if (equal? arg-ind i) arg-val (list-ref arg-list i)))
        ;             )
        ;         ]
        ;         [else (println-and-exit (format "# [zhash-panic] val-list-set: unsupported list type, got: ~a." arg-list))]
        ;     )
        ; )
        (define (val-list-set arg-val-list arg-ind arg-val)
            (when (or (not (integer? arg-ind)) (symbolic? arg-ind)) (println-and-exit (format "# [zhash-panic] val-list-set: arg-ind should be a concrete integer, got: ~a." arg-ind)))
            (cond
                ; (note) val-list here in rosette can be
                ; - a concrete list
                ; - a list of terms, e.g., (list (ite ...) (ite ...)), whose length is fixed
                ; - (note) by construction, it will NOT be a union
                [(list? arg-val-list)
                    ; here the list is either concrete or a list of symbolics
                    ; either way the length is fixed
                    (let ([n (length arg-val-list)])
                        (when (symbolic? n) (println-and-exit (format "# [zhash-panic] val-list-set: length of a list should not be symbolic, got: ~a." n)))
                        (for/list ([i (range n)]) (if (equal? arg-ind i) arg-val (list-ref arg-val-list i)))
                    )
                ]
                [else (println-and-exit (format "# [zhash-panic] val-list-set: unsupported list type, got: ~a." arg-val-list))]
            )
        )

        ; add an element to the end of the val-list
        ; see val-list-set for requirement details
        (define/public (val-list-append arg-val)
            (cond
                [(list? val-list) (set! val-list (reverse (cons arg-val (reverse val-list))))]
                [else (println-and-exit (format "# [zhash-panic] val-list-append: unsupported list type, got: ~a." val-list))]
            )
        )

        ; check the key-index-map for existence of a key
        ; - arg-key should be concrete, if not concrete, the caller should wrap a for/all first
        (define (zhash-key-exists? arg-key)
            (when (symbolic? arg-key) (println-and-exit (format "# [zhash-panic] zhash-key-exists?: arg-key should be concrete, got: ~a." arg-key)))
            (hash-has-key? key-index-map arg-key)
        )

        ; check the **factual** existence of a key
        ; if the corresponding value of this key is zvoid, it means the key is **factually** non-existent
        (define/public (zhash-has-key? arg-key)
            (cond
                [(symbolic? arg-key) (for/all ([dkey arg-key #:exhaustive]) (zhash-has-key? dkey))]
                [else
                    (if (zhash-key-exists? arg-key)
                        ; key exists in the key-index-map, check whether it's zvoid
                        (if (zvoid? (list-ref val-list (hash-ref key-index-map arg-key)))
                            #f ; zvoid will be treated as key doesn't exist
                            #t ; otherwise the key exists
                        )
                        ; key doesn't exist in the key-index-map, directly return #f
                        #f
                    )
                ]
            )
        )

        ; make sure arg-key exists in key-index-map
        ; if not, add it
        (define (secure-key arg-key)
            (printf "# securing key: ~a\n" arg-key)
            (cond
                [(symbolic? arg-key) (for/all ([dkey arg-key #:exhaustive]) (secure-key dkey))]
                [else
                    (when (not (zhash-key-exists? arg-key))
                        (let ([n (length val-list)])
                            (printf "  # length is: ~a\n" n)
                            (when (symbolic? n) (println-and-exit (format "# [zhash-panic] secure-key: length of val-list should not be symbolic, got: ~a." n)))
                            ; (fixme) you probably want to temporarily clear the vc here
                            (hash-set! key-index-map arg-key n) ; add the key to the key-index-map
                            (val-list-append zvoid) ; add zvoid to the val-list, secure a position
                            (printf "  # done, val-list is: ~a\n" val-list)
                        )
                    )
                ]
            )
        )

        ; (note) if the key doesn't exist, the path will authmatically be cut by rosette, which is expected
        (define/public (zhash-ref arg-key)
            (cond
                [(symbolic? arg-key) (for/all ([dkey arg-key]) (zhash-ref dkey))]
                [else (list-ref val-list (hash-ref key-index-map arg-key))]
            )
        )

        (define/public (zhash-set! arg-key arg-val)
            ; first secure all the keys
            ; this will update val-list to make sure of sufficient slots before actual filling of values
            (secure-key arg-key)
            ; then set the value
            (set! val-list (zhash-set val-list arg-key arg-val))
        )

        ; (note) if the key doesn't exist, the path will authmatically be cut by rosette, which is expected
        ; internal method, this returns a copy
        (define (zhash-set arg-val-list arg-key arg-val)
            (cond
                [(symbolic? arg-key) (for/all ([dkey arg-key]) (zhash-set arg-val-list dkey arg-val))]
                [else
                    (val-list-set arg-val-list (hash-ref key-index-map arg-key) arg-val)
                ]
            )
        )


    )
)