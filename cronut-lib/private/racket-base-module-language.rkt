#lang parendown racket/base

; cronut/private/racket-base-module-language.rkt
;
; The expansion-time module language corresponding to the language
; `#lang cronut/racket/base`, which defines a Racket module that has
; access to most of the bindings of `racket/base` while also having
; the ability to make Cronut declarations for things like cyclic
; module dependencies.

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


(require #/for-syntax racket/base)

(require #/for-syntax #/only-in cronut/private/shim
  shim-require-various)

(begin-for-syntax #/shim-require-various)

(require #/only-in cronut/private/shim shim-require-various)

(shim-require-various)


(provide #/except-out (all-from-out racket/base) #%module-begin)
(provide
  ; TODO: See if we should really provide these from here.
  (for-syntax cronut-declaration-macro?)
  (for-syntax gen:cronut-declaration-macro)
  (for-syntax cronut-declaration-macro-call)
  #%cronut-declaration
  (for-syntax expand-cronut-module-begin)
  example-cronut-declaration)
(provide #/rename-out [-#%module-begin #%module-begin])



(begin-for-syntax #/define-generics cronut-declaration-macro
  (cronut-declaration-macro-call cronut-declaration-macro stx decls))


(begin-for-syntax #/struct cronut-declaration-representation ()
  #:property prop:expansion-contexts (list 'module)
  #:property prop:procedure
  (fn self stx
    (error "encountered a Cronut declaration in a non-Cronut Racket module")))

(define-syntax #%cronut-declaration
  (cronut-declaration-representation))


(define-for-syntax (expand-cronut-module-begin decls)
  (expect decls (cons decl decls) #'(begin)
  #/syntax-parse
    (local-expand decl 'module
      (list
        
        ; Syntaxes that expect to be part of the stop list so they can
        ; cooperate with a module body.
        ;
        ; TODO: See which of the other syntaxes we should put here.
        ;
        #'define-values
        #'#%provide
        
        #'#%cronut-declaration))
    [
      ({~literal #%cronut-declaration} ~! {~and call (op . args)})
      ; TODO: See if we should add a `disappeared-use` syntax
      ; property here.
      (w- op (syntax-local-value #'op)
      #/expect (cronut-declaration-macro? op) #t
        (error "encountered a #%cronut-declaration with an operation that wasn't a Cronut declaration macro")
      #/cronut-declaration-macro-call op #'call decls)]
    [
      ({~literal begin} ~! begin-decls:expr ...)
      (expand-cronut-module-begin
        (append (syntax->list #'(begin-decls ...)) decls))]
    [
      (
        {~or
          
          ; Expression syntaxes that are part of both the
          ; `local-expand` default stop list and the syntax of
          ; module-level declarations in fully expanded programs.
          {~literal begin0}
          {~literal case-lambda}
          {~literal if}
          {~literal letrec-values}
          {~literal let-values}
          {~literal #%plain-app}
          {~literal #%plain-lambda}
          {~literal set!}
          {~literal #%top}
          {~literal #%variable-reference}
          {~literal with-continuation-mark}
          
          ; Expression syntaxes that are part of the `local-expand`
          ; default stop list but not part of the syntax of
          ; module-level declarations in fully expanded programs.
          {~literal #%expression}
          {~literal letrec-syntaxes+values}
          
          ; Expression syntaxes that are part of the syntax of
          ; module-level declarations in fully expanded programs but
          ; not part of the `local-expand` default stop list.
          {~literal quote}
          {~literal quote-syntax}
          
          ; Non-expression syntaxes that are part of the syntax of
          ; module-level declarations in fully expanded programs. Note
          ; that these are  not part of the `local-expand` default
          ; stop list.
          ;
          ; TODO: We should probably add several of these to the stop
          ; list to prevent recursive expansion of their subforms, and
          ; we should probably handle them with custom behavior.
          ;
          {~literal begin-for-syntax}
          {~literal define-syntaxes}
          {~literal define-values}
          {~literal #%declare}
          {~literal module}
          {~literal module*}
          {~literal #%provide}
          {~literal #%require}
          
          }
        . args)
      #`(begin #,decl #,(expand-cronut-module-begin decls))]
    [_ (error "Cronut internal error: unexpected local-expand result")]))

(begin-for-syntax #/struct module-begin-representation ()
  #:property prop:expansion-contexts (list 'module-begin)
  #:property prop:procedure
  (fn self stx
    (syntax-protect
    #/syntax-parse stx #/ (_ decl:expr ...)
    #/w- decl (expand-cronut-module-begin (syntax->list #'(decl ...)))
      #`(#%module-begin #,decl))))

(define-syntax -#%module-begin (module-begin-representation))


(begin-for-syntax #/struct example-cronut-declaration-representation
  ()
  #:property prop:expansion-contexts (list 'module)
  #:property prop:procedure
  (fn self stx
    (syntax-protect
      #`(#%cronut-declaration #,stx)))
  #:methods gen:cronut-declaration-macro
  [
    (define (cronut-declaration-macro-call self stx decls)
      #`(begin stx #,(expand-cronut-module-begin decls)))])

(define-syntax example-cronut-declaration
  (example-cronut-declaration-representation))
