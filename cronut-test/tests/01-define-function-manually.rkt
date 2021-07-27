#lang parendown racket/base

; cronut/tests/01-define-function-manually
;
; A unit test which defines a Cronut lexical unit using Racket code
; which has a function which another Racket module can call.
;
; This module is the Cronut lexical unit that defines the function.

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
  
  
  (define-for-syntax add-two-definer-spine
    (make-module-spine 'cronut 'tests '01-define-function-manually))
  
  (define-for-syntax add-two-declared-lexical-unit
    (declared-lexical-unit (set)
      (list #`#/declare-using-racket #,#/fn #/cronut-declaration
        (make-module-spine
          'cronut 'tests '01-define-function-manually)
        (list)
        (list #'add-two)
        #'#`
        (compiled-lexical-unit
          (hash 'add-two
            (compiled-lexical-unit-entry-for-single-argument-function
              #'x
              #'#`(#,#'#,add-two #,x))))
        #'#`
        (define (#,add-two x)
          (+ 2 x)))))
  
  
  (define-for-syntax add-two-declaration
    (dissect add-two-declared-lexical-unit
      (declared-lexical-unit _ #/list declaration-syntax)
    #/dissect (syntax->datum declaration-syntax)
      `(declare-using-racket ,get-declaration)
    #/get-declaration))
  
  ; TODO: Resolve the imports automatically, and generate the
  ; variables automatically as well (as depicted below).
  (define-for-syntax add-two-imports (list))
  (define-for-syntax add-two-locals (list #'0:add-two))
  #;
  (define-for-syntax add-two-locals
    (dissect add-two-declaration (cronut-declaration _ _ locals _ _)
    #/map (make-syntax-introducer) locals))
  
  (define-for-syntax add-two-compiled-expr
    (dissect add-two-declaration
      (cronut-declaration _ imports locals build-compiled _)
    #/w- imports
      (list-map imports #/dissectfn (list import _ _ _) import)
    #/with-syntax ([(imports ...) imports] [(locals ...) locals])
    #/apply
      (syntax-local-eval
        #`(lambda (imports ... locals ...) #,build-compiled))
      (append add-two-imports add-two-locals)))
  
  (define-for-syntax add-two-run-time-declaration
    (dissect add-two-declaration
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
      (append add-two-imports add-two-locals)))
  
  (define-syntax-parse-rule
    (add-two-run-compiled-expr add-two-compiled:id)
    #:with result (syntax-local-introduce add-two-compiled-expr)
    (define-for-syntax add-two-compiled result))
  
  (add-two-run-compiled-expr add-two-compiled)
  
  (define-syntax-parse-rule (add-two-run-run-time-declaration)
    
    #:with result
    (syntax-local-introduce add-two-run-time-declaration)
    
    result)
  
  
  (define-for-syntax lexical-unit-compile-time
    (module-contents-for-lexical-unit
      (just-value #/simplify-module-spine add-two-definer-spine)
      (here-bundle
        (hash
          (just-value #/simplify-module-spine add-two-definer-spine)
          add-two-declared-lexical-unit)
        (hash add-two-definer-spine add-two-compiled))))
  
  (add-two-run-run-time-declaration)
  
  )
