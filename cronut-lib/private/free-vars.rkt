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


(struct free-vars-table (phase quotes gets sets))

(define (make-free-vars-table phase)
  (free-vars-table
    phase
    (nothing)
    (make-immutable-bound-id-table phase)
    (make-immutable-bound-id-table phase)))

(define (make-free-vars-table-like table)
  (dissect table (free-vars-table phase _ _ _)
  #/make-free-vars-table phase))

(define (free-vars-table-update-quotes table update)
  (dissect table (free-vars-table phase quotes gets sets)
  #/free-vars-table phase (update quotes) gets sets))

(define (free-vars-table-update-gets table update)
  (dissect table (free-vars-table phase quotes gets sets)
  #/free-vars-table phase quotes (update gets) sets))

(define (free-vars-table-update-sets table update)
  (dissect table (free-vars-table phase quotes gets sets)
  #/free-vars-table phase quotes gets (update sets)))

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
  (dissect a (free-vars-table a-phase a-quotes a-gets a-sets)
  #/dissect b (free-vars-table b-phase b-quotes b-gets b-sets)
  #/dissect (equal? a-phase b-phase) #t
  #/free-vars-table a-phase
    (maybe-union a-quotes b-quotes elems-merge)
    (bound-id-table-union a-gets b-gets elems-merge)
    (bound-id-table-union a-sets b-sets elems-merge)))

(define (free-vars-table-minus a b)
  (dissect a (free-vars-table a-phase a-quotes a-gets a-sets)
  #/dissect b (free-vars-table b-phase b-quotes b-gets b-sets)
  #/dissect (equal? a-phase b-phase) #t
  #/free-vars-table a-phase
    (maybe-minus a-quotes b-quotes)
    (bound-id-table-minus a-gets b-gets)
    (bound-id-table-minus a-sets b-sets)))

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
      ({~literal let-values} ~! ([(var:id ...) val:expr] ...)
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
      ; TODO EXPANDER: See what we should do about `#%top` references.
      ; Maybe they're their own kind of free variable.
      (error "#%top: not implemented yet for Cronut")]
    [
      (
        {~literal #%variable-reference} ~!
        . {~and args {~or () (var:id) ({~literal #%top} . var:id)}})
      ; TODO EXPANDER: See what we should do about
      ; `#%variable-reference` references. Maybe they're their own
      ; kind of free variable.
      (error "#%variable-reference: not implemented yet for Cronut")]
    [
      (
        {~literal with-continuation-mark} ~! key:expr val:expr
        result:expr)
      (suppliable-map (process-exprs #'(key val result)) #/fn exprs
        (rearm #`(with-continuation-mark #,@exprs)))]
    [_ (error "Cronut internal error: unexpected fully expanded expression")]))
