#lang parendown racket/base

; cronut/private/cronut-from-racket
;
; Miscellaneous implementation details of Cronut for the purpose of
; importing Cronut definitions from Racket modules.

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

(require #/for-syntax cronut/private/cronut)
(require #/for-syntax cronut/private/shim)

(begin-for-syntax #/shim-require-various)

(require cronut/private/shim)

(shim-require-various)


(provide
  import-cronut-single-argument-function)


(define-syntax-parse-rule
  (import-cronut-single-argument-function
    spine-expr:expr from-id-expr:expr var:id)
  
  (begin
    
    (define-for-syntax definer-spine spine-expr)
    (define-for-syntax from-id from-id-expr)
    
    (define-syntax-parse-rule (require-definer definer:id)
      
      #:with module-path
      (expect (simplify-module-spine definer-spine) (just spine)
        ; TODO: Improve this error message.
        (error "Tried to import from a Cronut lexical unit whose module spine couldn't be converted to a Racket module path")
      #/simplified-module-spine->racket-module-path spine)
      
      (require #/only-in
        (submod module-path :%private/generated:cronut:lexical-unit)
        [lexical-unit-compile-time definer]))
    
    (require-definer definer)
    
    (define-syntax (require-definer-host stx)
      (syntax-protect
      #/syntax-parse stx #/ (require-definer-host definer-host:id)
      #/dissect definer (module-contents-for-lexical-unit _ bundle)
      #/mat bundle (here-bundle _ _)
        #'(define-for-syntax definer-host definer)
      #/dissect bundle (elsewhere-bundle host _)
        (with-syntax
          (
            [
              module-path
              (simplified-module-spine->racket-module-path host)])
          #'
          (require #/only-in
            (submod module-path
              :%private/generated:cronut:lexical-unit)
            [lexical-unit-compile-time definer-host]))))
    
    (require-definer-host definer-host)
    
    (define-for-syntax entry-for-import
      (dissect definer-host
        (module-contents-for-lexical-unit _
          (here-bundle _ compiled-lexical-units))
      #/dissect (hash-ref compiled-lexical-units definer-spine)
        (compiled-lexical-unit functions)
      ; TODO: Handle the case where there is no function by this name.
      #/hash-ref functions from-id))
    
    (define-for-syntax transform-import
      (dissect entry-for-import
        (compiled-lexical-unit-entry-for-single-argument-function
          x body)
      #/syntax-local-eval #`(lambda (#,x) #,body)))
    
    (define-syntax-parse-rule (var x:expr)
      #:with result (transform-import #'x)
      result)
    
    ))
