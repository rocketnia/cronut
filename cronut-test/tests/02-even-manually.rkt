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
  
  
  (define-for-syntax lexical-unit-compile-time
    (module-contents-for-lexical-unit
      (just-value #/simplify-module-spine even-definer-spine)
      (here-bundle
        (hash
          (just-value #/simplify-module-spine even-definer-spine)
          even-declared-lexical-unit
          (just-value #/simplify-module-spine odd-definer-spine)
          odd-declared-lexical-unit)
        ; TODO: Compute these `compiled-lexical-unit?` values by
        ; compiling the `declared-lexical-unit?` values.
        (hash
          even-definer-spine
          (compiled-lexical-unit
            (hash 'is-even?
              (compiled-lexical-unit-entry-for-single-argument-function
                #'x
                #'#`(0:is-even? #,x))))
          odd-definer-spine
          (compiled-lexical-unit
            (hash 'is-odd?
              (compiled-lexical-unit-entry-for-single-argument-function
                #'x
                #'#`(1:is-odd? #,x))))))))
  
  ; TODO: Compute the run-time declarations below by compiling the
  ; `declared-lexical-unit?` values.
  
  (define (0:is-even? x)
    (or (zero? x) (1:is-odd? #/sub1 x)))
  
  (define (1:is-odd? x)
    (and (not #/zero? x) (0:is-even? #/sub1 x)))
  
  )
