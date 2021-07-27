#lang parendown racket/base

; cronut/tests/02-even-manually
;
; A unit test which defines two mutually recursive Cronut lexical
; units using Racket code and calls the function from the non-host
; lexical unit from another Racket module.
;
; This module is the host Cronut lexical unit. It defines the
; `is-even?` function.

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


; (We provide nothing from this module.)


(module :%private/generated:cronut:lexical-unit racket/base
  
  (require #/for-syntax racket/base)
  
  (require #/for-syntax cronut/private/cronut)
  (require #/for-syntax cronut/private/shim)
  
  (begin-for-syntax #/shim-require-various)
  
  (require cronut/private/shim)
  
  (shim-require-various)
  
  
  (provide #/for-syntax lexical-unit-compile-time)
  
  
  (define-for-syntax even-definer-spine
    (make-module-spine 'cronut 'tests '02-even-manually))
  
  (define-for-syntax even-declared-lexical-unit
    (declared-lexical-unit (set)
      (list #`#/declare-using-racket #,#/fn #/cronut-declaration
        (make-module-spine 'cronut 'tests '02-even-manually)
        (list
          (list #'is-odd? 'single-argument-function
            (make-module-spine 'cronut 'tests '02-odd-manually)
            'is-odd?))
        (list #'is-even?)
        #'#`
        (compiled-lexical-unit
          (hash 'is-even?
            (compiled-lexical-unit-entry-for-single-argument-function
              #'x
              #'#`(#,#'#,is-even? #,x))))
        #'#`
        (define (#,is-even? x)
          (or (zero? x) (#,is-odd? #/sub1 x))))))
  
  
  (define-for-syntax odd-definer-spine
    (make-module-spine 'cronut 'tests '02-odd-manually))
  
  (define-syntax-parse-rule (require-odd-definer odd-definer:id)
    
    #:with module-path
    (expect (simplify-module-spine odd-definer-spine) (just spine)
      ; TODO: Improve this error message.
      (error "Tried to import from a Cronut lexical unit whose module spine couldn't be converted to a Racket module path")
    #/simplified-module-spine->racket-module-path spine)
    
    (require #/only-in
      (submod module-path :%private/generated:cronut:lexical-unit)
      [lexical-unit-compile-time odd-definer]))
  
  (require-odd-definer odd-definer)
  
  (define-for-syntax odd-declared-lexical-unit
    (dissect odd-definer
      (module-contents-for-lexical-unit _
        (elsewhere-bundle _ odd-declared-lexical-unit))
      odd-declared-lexical-unit))
  
  
  (define-for-syntax even-declaration
    (dissect even-declared-lexical-unit
      (declared-lexical-unit _ #/list declaration-syntax)
    #/dissect (syntax->datum declaration-syntax)
      `(declare-using-racket ,get-declaration)
    #/get-declaration))
  
  (define-for-syntax even-introduce (make-syntax-introducer))
  (define-for-syntax even-imports
    (dissect even-declaration (cronut-declaration _ imports _ _ _)
    #/list-map imports #/dissectfn (list import _ _ _)
      (even-introduce import)))
  (define-for-syntax even-locals
    (dissect even-declaration (cronut-declaration _ _ locals _ _)
    #/map even-introduce locals))
  
  (define-for-syntax even-compiled-expr
    (dissect even-declaration
      (cronut-declaration _ imports locals build-compiled _)
    #/w- imports
      (list-map imports #/dissectfn (list import _ _ _) import)
    #/with-syntax ([(imports ...) imports] [(locals ...) locals])
    #/apply
      (syntax-local-eval
        #`(lambda (imports ... locals ...) #,build-compiled))
      (append even-imports even-locals)))
  
  (define-for-syntax even-run-time-declaration
    (dissect even-declaration
      (cronut-declaration
        _ imports locals _ build-run-time-declaration)
    #/w- imports
      (list-map imports #/dissectfn (list import _ _ _) import)
    #/with-syntax ([(imports ...) imports] [(locals ...) locals])
    #/apply
      (syntax-local-eval
        #`
        (lambda (imports ... locals ...)
          #,build-run-time-declaration))
      (append even-imports even-locals)))
  
  (define-syntax-parse-rule
    (even-run-compiled-expr even-compiled:id)
    #:with result (syntax-local-introduce even-compiled-expr)
    (define-for-syntax even-compiled result))
  
  (even-run-compiled-expr even-compiled)
  
  (define-syntax-parse-rule (even-run-run-time-declaration)
    
    #:with result
    (syntax-local-introduce even-run-time-declaration)
    
    result)
  
  
  (define-for-syntax odd-declaration
    (dissect odd-declared-lexical-unit
      (declared-lexical-unit _ #/list declaration-syntax)
    #/dissect (syntax->datum declaration-syntax)
      `(declare-using-racket ,get-declaration)
    #/get-declaration))
  
  (define-for-syntax odd-introduce (make-syntax-introducer))
  (define-for-syntax odd-imports
    (dissect odd-declaration (cronut-declaration _ imports _ _ _)
    #/list-map imports #/dissectfn (list import _ _ _)
      (odd-introduce import)))
  (define-for-syntax odd-locals
    (dissect odd-declaration (cronut-declaration _ _ locals _ _)
    #/map odd-introduce locals))
  
  (define-for-syntax odd-compiled-expr
    (dissect odd-declaration
      (cronut-declaration _ imports locals build-compiled _)
    #/w- imports
      (list-map imports #/dissectfn (list import _ _ _) import)
    #/with-syntax ([(imports ...) imports] [(locals ...) locals])
    #/apply
      (syntax-local-eval
        #`(lambda (imports ... locals ...) #,build-compiled))
      (append odd-imports odd-locals)))
  
  (define-for-syntax odd-run-time-declaration
    (dissect odd-declaration
      (cronut-declaration
        _ imports locals _ build-run-time-declaration)
    #/w- imports
      (list-map imports #/dissectfn (list import _ _ _) import)
    #/with-syntax ([(imports ...) imports] [(locals ...) locals])
    #/apply
      (syntax-local-eval
        #`
        (lambda (imports ... locals ...)
          #,build-run-time-declaration))
      (append odd-imports odd-locals)))
  
  (define-syntax-parse-rule
    (odd-run-compiled-expr odd-compiled:id)
    #:with result (syntax-local-introduce odd-compiled-expr)
    (define-for-syntax odd-compiled result))
  
  (odd-run-compiled-expr odd-compiled)
  
  (define-syntax-parse-rule (odd-run-run-time-declaration)
    
    #:with result
    (syntax-local-introduce odd-run-time-declaration)
    
    result)
  
  
  ; TODO: This shares some implementation details with
  ; `import-cronut-single-argument-function`. Factor these out.
  (define-syntax-parse-rule
    (resolve-single-argument-function
      var:id compiled-expr:expr from-id-expr:expr)
    (begin
      (define-for-syntax compiled compiled-expr)
      (define-for-syntax from-id from-id-expr)
      
      (define-for-syntax entry-for-import
        (dissect compiled (compiled-lexical-unit functions)
        ; TODO: Handle the case where there is no function by this
        ; name.
        #/hash-ref functions from-id))
      
      (define-for-syntax transform-import
        (dissect entry-for-import
          (compiled-lexical-unit-entry-for-single-argument-function
            x body)
        #/syntax-local-eval #`(lambda (#,x) #,body)))
      
      (define-syntax-parse-rule (var x:expr)
        #:with result (transform-import #'x)
        result)))
  
  
  (define-for-syntax compiled-id-hash
    (hash
      even-definer-spine #'even-compiled
      odd-definer-spine #'odd-compiled))
  
  
  (define-syntax (resolve-imports stx)
    (syntax-protect
    #/syntax-parse stx #/ (_ to-ids:expr declaration:expr)
    #/w- to-ids (syntax-local-eval #'to-ids)
    #/dissect (syntax-local-eval #'declaration)
      (cronut-declaration _ imports _ _ _)
    #/with-syntax
      (
        [(to-id ...) (map syntax-local-introduce to-ids)]
        [
          (compiled-expr ...)
          (list-map imports
            (dissectfn (list _ 'single-argument-function spine _)
              (hash-ref compiled-id-hash spine)))]
        [
          (from-id ...)
          (list-map imports
            (dissectfn (list _ 'single-argument-function _ from-id)
              from-id))])
      #'
      (begin
        (resolve-single-argument-function
          to-id compiled-expr 'from-id)
        ...)))
  
  
  (resolve-imports even-imports even-declaration)
  (resolve-imports odd-imports odd-declaration)
  
  
  (define-for-syntax lexical-unit-compile-time
    (module-contents-for-lexical-unit
      (just-value #/simplify-module-spine even-definer-spine)
      (here-bundle
        (hash
          (just-value #/simplify-module-spine even-definer-spine)
          even-declared-lexical-unit
          (just-value #/simplify-module-spine odd-definer-spine)
          odd-declared-lexical-unit)
        (hash
          even-definer-spine even-compiled
          odd-definer-spine odd-compiled))))
  
  (even-run-run-time-declaration)
  (odd-run-run-time-declaration)
  
  )
