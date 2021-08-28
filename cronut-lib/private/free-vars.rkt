#lang parendown racket/base

; cronut/private/free-vars
;
; A utility for traversing a fully expanded Racket expression and
; replacing its free variable uses.

;   Copyright 2021 The Cronut Authors
;
;   Licensed under the Apache License, Version 2.0 (the "License");
;   you may not use this file except in compliance with the License.
;   You may obtain a copy of the License at
;
;       http://www.apache.org/licenses/LICENSE-2.0
;
;   Unless required by applicable law or agreed to in writing,
;   software distributed under the License is distributed on an
;   "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
;   either express or implied. See the License for the specific
;   language governing permissions and limitations under the License.


(require #/only-in cronut/private/shim shim-require-various)

(shim-require-various)


; TODO: Provide things from here.


(struct free-vars-table (phase maybes tables))

(define (make-free-vars-table phase)
  (free-vars-table
    phase
    (hash
      'quotes (nothing)
      'anonvarrefs (nothing)
      
      ; TODO: Consider adding uses of
      ;
      ;  (#%plain-app variable-reference-from-unsafe?
      ;    (#%variable-reference))
      ;
      ; as their own kind of free variable here. Actually, we might
      ; be able to compute this statically in Cronut.
      
      )
    (hash
      'gets (make-immutable-bound-id-table phase)
      'sets (make-immutable-bound-id-table phase)
      'tops (make-immutable-bound-id-table phase)
      
      ; Variable references seem to keep track of a few things:
      ;
      ;  - The variable's phase. (We can compute this statically.)
      ;
      ;  - Whether the variable is constant. (We can compute this
      ;    statically, with a second traversal over the syntax.)
      ;
      ;  - Whether the variable reference form was compiled in unsafe
      ;    mode. (Can we compute this statically?)
      ;
      ;  - If we obtained the reference using
      ;    `(#%variable-reference id)`, whether the variable refers to
      ;    a module binding. (We can compute this statically.)
      ;
      ;  - If we obtained the reference using
      ;    `(#%variable-reference id)`, if the variable refers to a
      ;    module binding, and if that module is being compiled, the
      ;    module path of that module (giving information like the
      ;    module path index, the module source, the namespace, etc.).
      ;    (Can we compute this statically?)
      ;
      ;  - If the variable refers to a module binding, the offset of
      ;    the module's base phase relative to the variable's phase.
      ;    (We can compute this statically.) (Does this depend on how
      ;    we obtained the reference? If we obtained it using
      ;    `(#%variable-reference (#%top . id))`, do we get the offset
      ;    of the base phase of the module that compiled the form
      ;    instead?)
      ;
      ;  - If we obtained the reference using
      ;    `(#%variable-reference (#%top . id))`, whether that form
      ;    was compiled as part of a module.
      ;
      ;  - If we obtained the reference using
      ;    `(#%variable-reference (#%top . id))`, and if that form was
      ;    compiled as part of a module that's being compiled, the
      ;    module path of that module (giving various information).
      ;    (Can we compute this statically?)
      ;
      ;  - (Variable references tie into the BC extension API somehow.
      ;    What does that entail?)
      ;
      ;  - (Are we missing some things?)
      ;
      ; (TODO: Answer some of these questions.)
      ;
      ; Virtually all of this seems to be statically computable, which
      ; might even be the point. The parts that refer to module
      ; bindings probably become trivial when `id` is a local variable
      ; name, too. The one part that seems like it could vary between
      ; the outside and the inside of a local variable scope is the
      ; question of whether the variable going by that name is
      ; constant (`variable-reference-constant?`). But since there is
      ; one thing that varies that way, we maintain information about
      ; `#%variable-reference` occurrences using variable-name-keyed
      ; tables rather than sharing it across all variable names.
      ;
      'varrefs (make-immutable-bound-id-table phase)
      'topvarrefs (make-immutable-bound-id-table phase))))

(define (make-free-vars-table-like table)
  (dissect table (free-vars-table phase _ _)
  #/make-free-vars-table phase))

(define (free-vars-table-subtable-maybe table maybe-key)
  (dissect table (free-vars-table phase maybes tables)
  #/hash-ref maybes maybe-key))
(define (free-vars-table-update-maybe table maybe-key update)
  (dissect table (free-vars-table phase maybes tables)
  #/free-vars-table phase
    (hash-update maybes maybe-key #/fn quotes #/update quotes)
    tables))

(define (free-vars-table-subtable-table table table-key)
  (dissect table (free-vars-table phase maybes tables)
  #/hash-ref tables table-key))
(define (free-vars-table-update-table table table-key update)
  (dissect table (free-vars-table phase maybes tables)
  #/free-vars-table phase
    maybes
    (hash-update tables table-key #/fn gets #/update gets)))

(define (free-vars-table-quotes table update)
  (free-vars-table-subtable-maybe table 'quotes))
(define (free-vars-table-update-quotes table update)
  (free-vars-table-update-maybe table 'quotes update))

(define (free-vars-table-anonvarrefs table update)
  (free-vars-table-subtable-maybe table 'anonvarrefs))
(define (free-vars-table-update-anonvarrefs table update)
  (free-vars-table-update-maybe table 'anonvarrefs update))

(define (free-vars-table-gets table update)
  (free-vars-table-subtable-table table 'gets))
(define (free-vars-table-update-gets table update)
  (free-vars-table-update-table table 'gets update))

(define (free-vars-table-sets table update)
  (free-vars-table-subtable-table table 'sets))
(define (free-vars-table-update-sets table update)
  (free-vars-table-update-table table 'sets update))

(define (free-vars-table-tops table update)
  (free-vars-table-subtable-table table 'tops))
(define (free-vars-table-update-tops table update)
  (free-vars-table-update-table table 'tops update))

(define (free-vars-table-varrefs table update)
  (free-vars-table-subtable-table table 'varrefs))
(define (free-vars-table-update-varrefs table update)
  (free-vars-table-update-table table 'varrefs update))

(define (free-vars-table-topvarrefs table update)
  (free-vars-table-subtable-table table 'topvarrefs))
(define (free-vars-table-update-topvarrefs table update)
  (free-vars-table-update-table table 'topvarrefs update))

(define (bound-id-table-ref-maybe table key)
  (define sentinel (box #f))
  (define result (bound-id-table-ref table key #/fn sentinel))
  (maybe-if (not #/eq? sentinel result) #/fn result))

(define (maybe-union a b elems-merge)
  (expect a (just a-elem) b
  #/expect b (just b-elem) a
  #/just #/elems-merge a-elem b-elem))

(define (maybe-minus a b)
  (mat b (nothing)
    a
    (nothing)))

(define (bound-id-table-union a b elems-merge)
  (define-values (smaller larger adjusted-elems-merge)
    (if (<= (bound-id-table-count a) (bound-id-table-count b))
      (values a b elems-merge)
      (values b a (fn b a #/elems-merge a b))))
  (for/fold
    ([result larger])
    ([(k smaller-v) (in-bound-id-table smaller)])
    (bound-id-table-set result k
      (mat (bound-id-table-ref-maybe result) (just larger-v)
        (adjusted-elems-merge smaller-v larger-v)
        smaller-v))))

(define (bound-id-table-minus a b)
  (for/fold ([result a]) ([(k v) (in-bound-id-table b)])
    (bound-id-table-remove result k)))

(define (free-vars-table-union a b elems-merge)
  (dissect a (free-vars-table a-phase a-maybes a-tables)
  #/dissect b (free-vars-table b-phase b-maybes b-tables)
  #/dissect (equal? a-phase b-phase) #t
  #/free-vars-table a-phase
    (hash-union a-maybes b-maybes #/fn a-maybe b-maybe
      (maybe-union a-maybe b-maybe elems-merge))
    (hash-union a-tables b-tables #/fn a-table b-table
      (bound-id-table-union a-table b-table elems-merge))))

(define (free-vars-table-minus a b)
  (dissect a (free-vars-table a-phase a-maybes a-tables)
  #/dissect b (free-vars-table b-phase b-maybes b-tables)
  #/dissect (equal? a-phase b-phase) #t
  #/free-vars-table a-phase
    (for/hash ([(k a-maybe) (in-hash a-maybes)])
      (define b-maybe (hash-ref b-maybes k))
      (maybe-minus a-maybe b-maybe))
    (for/hash ([(k a-table) (in-hash a-tables)])
      (define b-table (hash-ref b-tables k))
      (bound-id-table-minus a-table b-table))))

(struct suppliable (free-vars-set populate))

(define (suppliable-list-distribute phase list-of-suppliables)
  (define free-vars-set
    (for/fold
      ([state (make-free-vars-table phase)])
      ([s (in-list list-of-suppliables)])
      (dissect s (suppliable free-vars-set populate)
      #/free-vars-table-union state free-vars-set #/fn a b
        (append a b))))
  (define populates
    (list-map list-of-suppliables
    #/dissectfn (suppliable free-vars-set populate)
      populate))
  (suppliable free-vars-set #/fn free-vars-map
    (list-map populates #/fn populate #/populate free-vars-map)))

(define (suppliable-done phase populated-result)
  (suppliable (make-free-vars-table phase) #/fn table
    populated-result))

(define (suppliable-map prefix populated-to-suffix)
  (dissect prefix (suppliable free-vars-set populate)
  #/suppliable free-vars-set #/fn table
    (populated-to-suffix #/populate table)))

(define here-inspector (current-inspector))

(define (free-vars-table-add-formals table formals)
  (w- add
    (fn var
      (w- table
        (free-vars-table-update-gets table #/fn gets
          (bound-id-table-set gets var #/fn expr expr))
      #/w- table
        (free-vars-table-update-sets table #/fn sets
          (bound-id-table-set sets var #/fn val make-set
            (make-set val)))
      #/w- table
        (free-vars-table-update-tops table #/fn tops
          (bound-id-table-set tops var #/fn expr expr))
      #/w- table
        (free-vars-table-update-varrefs table #/fn varrefs
          (bound-id-table-set varrefs var #/fn val make-varref
            (make-varref val)))
      #/w- table
        (free-vars-table-update-topvarrefs table #/fn topvarrefs
          (bound-id-table-set topvarrefs var #/fn val make-topvarref
            (make-topvarref val)))
        table))
  #/syntax-parse formals
    [_:id (add formals)]
    [() table]
    [
      (arg:id . formals)
      (free-vars-table-add-formals (add #'arg) #'formals)]))

(define (free-vars-table-remove-formals table formals)
  (free-vars-table-minus table
    (free-vars-table-add-formals (make-free-vars-table-like table)
      formals)))

; TODO: Use this. Test this.
(define (find-free-vars phase expr-stx)
  (w- disarmed-expr (syntax-disarm expr-stx here-inspector)
  ; TODO: We choose to pass a false value here to ignore the
  ; `taint-mode` of the syntax we're rearming. It probably doesn't
  ; matter, but let's make sure it doesn't matter at some point.
  ; TODO: Wherever we use `rearm`, also transfer properties and
  ; location.
  #/w- rearm (fn s #/syntax-rearm s disarmed-expr #f)
  #/w- process-exprs
    (fn exprs
      (suppliable-list-distribute
        (list-map (syntax->list exprs) #/fn expr
          (find-free-vars expr))))
  #/w- process-formals-and-body
    (fn formals-and-body
      (syntax-parse formals-and-body #/ (formals body:expr ...+)
      #/dissect (process-exprs #'(body ...))
        (suppliable free-vars-set populate)
      #/w- formals
        (free-vars-table-add-formals (make-free-vars-table phase)
          #'formals)
      #/suppliable
        (free-vars-table-minus free-vars-set formals)
        (fn table
          (w- table (free-vars-table-union table formals #/fn a b b)
            #`(formals #,@(populate table))))))
  #/syntax-parse disarmed-expr
    [
      _:id
      (suppliable
        (free-vars-table-update-gets (make-free-vars-table phase)
        #/fn gets
          (bound-id-table-set gets disarmed-expr #/list expr-stx))
      #/fn table
        (rearm #/
          (bound-id-table-ref (free-vars-table-gets table)
            disarmed-expr)
          disarmed-expr))]
    [
      ({~literal begin} ~! expr:expr ...+)
      (suppliable-map (process-exprs #'(expr ...)) #/fn exprs
        (rearm #`(begin #,@exprs)))]
    [
      ({~literal begin0} ~! expr:expr ...+)
      (suppliable-map (process-exprs #'(expr ...)) #/fn exprs
        (rearm #`(begin0 #,@exprs)))]
    [
      ({~literal case-lambda} ~! cases ...)
      (suppliable-map
        (suppliable-list-distribute
          (list-map (syntax->list #'(cases ...)) #/fn case
            (process-formals-and-body case)))
      #/fn cases
        (rearm #`(case-lambda #,@cases)))]
    [
      ({~literal if} ~! condition:expr then:expr else:expr)
      (suppliable-map (process-exprs #'(condition then else))
      #/fn exprs
        (rearm #`(if #,@exprs)))]
    [
      (
        {~literal letrec-values} ~! ([(var:id ...) val:expr] ...)
        body:expr ...+)
      (suppliable-map
        (suppliable-list-distribute #/list
          (process-formals-and-body
            #'(({~@ var ...} ...) val ...))
          (process-formals-and-body
            #'(({~@ var ...} ...) body ...)))
      #/dissectfn (list vals vars-and-body)
        (with-syntax ([(_ val ...) vals] [(_ body ...) vars-and-body])
        #/rearm #`(letrec-values ([(var ...) val] ...) body ...)))]
    [
      (
        {~literal let-values} ~! ([(var:id ...) val:expr] ...)
        body:expr ...+)
      (suppliable-map
        (suppliable-list-distribute #/list
          (process-exprs #'(val ...))
          (process-formals-and-body #'(({~@ var ...} ...) body ...)))
      #/dissectfn (list vals vars-and-body)
        (with-syntax ([(val ...) vals] [(_ body ...) vars-and-body])
        #/rearm #`(let-values ([(var ...) val] ...) body ...)))]
    [
      ({~literal #%plain-app} ~! func:expr args:expr ...)
      (suppliable-map (process-exprs #'(func args ...))
      #/fn exprs
        (rearm #`(#%plain-app #,@exprs)))]
    [
      ({~literal #%plain-lambda} . formals-and-body)
      (process-formals-and-body #'formals-and-body
      #/fn formals-and-body
        (rearm #`(#%plain-lambda #,@formals-and-body)))]
    [
      ({~literal set!} ~! var:id val:expr)
      (suppliable-map
        (suppliable-list-distribute #/list
          (suppliable
            (free-vars-table-update-sets (make-free-vars-table phase)
            #/fn sets
              (bound-id-table-set sets #'var #/list #'var))
            (fn table
              (bound-id-table-ref (free-vars-table-sets table)
                #'var)))
          (find-free-vars #'val))
      #/dissectfn (list make-set val)
        (rearm #/make-set val #/fn val #`(set! var #,val)))]
    [
      ({~literal quote} ~! datum)
      (suppliable-done phase expr-stx)]
    [
      (
        {~literal quote-syntax} ~! datum
        . {~and maybe-local {~or () (#:local)}})
      (suppliable
        (free-vars-table-update-quotes (make-free-vars-table phase)
        #/fn quotes
          (just #/list expr-stx))
      #/fn table
        (w- make-quote-syntax
          (just-value #/free-vars-table-quotes table)
        #/rearm #/make-quote-syntax #'datum #/fn datum
          #`(quote-syntax #,datum . maybe-local)))]
    [
      ({~literal #%top} ~! . var:id)
      (suppliable
        (free-vars-table-update-tops
          (make-free-vars-table phase)
        #/fn tops
          (bound-id-table-set tops #'var #/list #'var))
      #/fn table
        (w- make-top
          (bound-id-table-ref (free-vars-table-tops table) #'var)
        #/rearm #/make-top disarmed-expr))]
    [
      ({~literal #%variable-reference} . args)
      (syntax-parse #'args
        [
          ()
          (suppliable
            (free-vars-table-update-anonvarrefs
              (make-free-vars-table phase)
            #/fn anonvarrefs
              (just #/list expr-stx))
          #/fn table
            (w- make-anonvarref
              (just-value #/free-vars-table-anonvarrefs table)
            #/rearm #/make-anonvarref disarmed-expr))]
        [
          (var:id)
          (suppliable
            (free-vars-table-update-varrefs
              (make-free-vars-table phase)
            #/fn varrefs
              (bound-id-table-set varrefs #'var #/list #'var))
          #/fn table
            (w- make-varref
              (bound-id-table-ref (free-vars-table-varrefs table)
                #'var)
            #/rearm #/make-varref disarmed-expr))]
        [
          (({~literal #%top} . var:id))
          (suppliable
            (free-vars-table-update-topvarrefs
              (make-free-vars-table phase)
            #/fn topvarrefs
              (bound-id-table-set topvarrefs #'var #/list #'var))
          #/fn table
            (w- make-topvarref
              (bound-id-table-ref (free-vars-table-topvarrefs table)
                #'var)
            #/rearm #/make-topvarref disarmed-expr))])]
    [
      (
        {~literal with-continuation-mark} ~! key:expr val:expr
        result:expr)
      (suppliable-map (process-exprs #'(key val result)) #/fn exprs
        (rearm #`(with-continuation-mark #,@exprs)))]
    [_ (error "Cronut internal error: unexpected fully expanded expression")]))
