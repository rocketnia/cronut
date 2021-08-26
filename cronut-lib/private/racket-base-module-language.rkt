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
  #/w- expanded-decl
    (local-expand decl 'module
      (list
        
        ; Non-expression syntaxes that are part of the syntax of
        ; module-level declarations in fully expanded programs. Most,
        ; if not all of these, expect to be handled specially by
        ; whatever is partially expanding the module. (Perhaps
        ; `module` doesn't expect to be handled specially.) Note that
        ; these are not part of the `local-expand` default stop list.
        ;
        #'begin-for-syntax
        #'define-syntaxes
        #'define-values
        #'#%declare
        #'module
        #'module*
        #'#%provide
        #'#%require
        
        ; Our own extension to the module-level declaration syntaxes.
        #'#%cronut-declaration))
  #/w- disarmed-expanded-decl
    (syntax-disarm expanded-decl (current-inspector))
  #/w- use-normal-semantics
    (fn
      #`(begin #,decl #,(expand-cronut-module-begin decls)))
  #/syntax-parse disarmed-expanded-decl
    [
      ({~literal #%cronut-declaration} . args)
      (syntax-parse disarmed-expanded-decl #/
        (_ {~and call (op . args)})
      ; TODO: See if we should add a `disappeared-use` syntax
      ; property here.
      #/w- op (syntax-local-value #'op)
      #/expect (cronut-declaration-macro? op) #t
        (error "encountered a #%cronut-declaration with an operation that wasn't a Cronut declaration macro")
      #/cronut-declaration-macro-call op #'call decls)]
    [
      ({~literal begin} . args)
      (syntax-parse disarmed-expanded-decl #/ (_ begin-decls:expr ...)
      #/expand-cronut-module-begin
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
          
          }
        . args)
      
      ; TODO EXPANDER: Actually, we won't want to use the normal
      ; semantics for expressions. We'll want to code-walk their fully
      ; expanded forms to replace uses of `quote-syntax` so that they
      ; construct syntax objects that refer to the particular
      ; argument-equipped module variant we're instantiating.
      ;
      (use-normal-semantics)
;      (error "expression at the module level: not implemented yet for Cronut")
      ]
    [
      ({~literal #%declare} . args)
      
      ; TODO EXPANDER: See if we really want to use the normal
      ; semantics for `#%declare`. Perhaps some segments of a module
      ; can be cross-phase persistent, unsafe, and/or free of
      ; `module->namespace` lexical information.
      ;
      (use-normal-semantics)]
    [
      ({~literal begin-for-syntax} . args)
      ; TODO EXPANDER
      (error "begin-for-syntax: not implemented yet for Cronut")]
    [
      ({~literal define-syntaxes} . args)
      ; TODO EXPANDER
      (error "define-syntaxes: not implemented yet for Cronut")]
    [
      ({~literal define-values} . args)
      
      ; TODO EXPANDER: Actually, we won't want to use the normal
      ; semantics for `define-values`. We'll want to keep track of it
      ; for use in various argument-equipped variants of the module,
      ; and we'll want to replace all the identifiers with ones that
      ; have a scope specific to the immediately compiled variant.
      ; We'll also want to code-walk their fully expanded forms to
      ; replace uses of `quote-syntax` so that they construct syntax
      ; objects that refer to the particular argument-equipped module
      ; variant we're instantiating.
      ;
      (use-normal-semantics)
;      (error "define-values: not implemented yet for Cronut")
      ]
    [
      ({~literal module} . args)
      
      ; Submodules that don't have access to their enclosing module
      ; don't need to interact with our Cronut module semantics.
      ;
      ; TODO EXPANDER: But do we want to record the fact that they
      ; exist so we can make identical submodules on other variants of
      ; the enclosing module?
      ;
      (use-normal-semantics)]
    [
      ({~literal module*} . args)
      (syntax-parse disarmed-expanded-decl
        [
          (_ name #f . args)
          ; TODO EXPANDER
          (error "module* with #f to give it initial access to the enclosing module: not implemented yet for Cronut")]
        [
          (_ . args)
          
          ; Submodules that have access to their enclosing module, but
          ; that only access it explicitly (e.g., through `require`)
          ; rather than having it as their initial import, don't need
          ; special care to interact with our Cronut module semantics.
          ; Either they'll use a Cronut-aware variant of `require`, or
          ; they'll be Cronut modules themselves.
          ;
          ; TODO EXPANDER: But do we want to record the fact that they
          ; exist so we can make identical submodules on other
          ; variants of the enclosing module?
          ;
          (use-normal-semantics)])]
    [
      ({~literal #%provide} . args)
      
      ; TODO EXPANDER: Actually, we won't want to use the normal
      ; semantics for `#%provide`. We'll want to keep track of it for
      ; use in various argument-equipped variants of the module, and
      ; we'll want to replace all the identifiers with ones that have
      ; a scope specific to the immediately compiled variant.
      ;
      (use-normal-semantics)
;      (error "#%provide: not implemented yet for Cronut")
      ]
    [
      ({~literal #%require} . args)
      
      ; TODO EXPANDER: Actually, we won't want to use the normal
      ; semantics for `#%require`. We'll want to keep track of it for
      ; use in various argument-equipped variants of the module, and
      ; we'll want to replace all the identifiers with ones that have
      ; a scope specific to the immediately compiled variant.
      ;
      (use-normal-semantics)
;      (error "#%require: not implemented yet for Cronut")
      ]
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
      (syntax-protect
      #/syntax-parse stx #/ (_ decl:expr)
        #`(begin decl #,(expand-cronut-module-begin decls))))])

(define-syntax example-cronut-declaration
  (example-cronut-declaration-representation))
