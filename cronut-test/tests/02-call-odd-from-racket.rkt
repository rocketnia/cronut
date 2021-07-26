#lang parendown racket/base

; cronut/tests/02-call-odd-from-racket
;
; A unit test which defines two mutually recursive Cronut lexical
; units using Racket code and calls the function from the non-host
; lexical unit from another Racket module.
;
; This module is the Racket consumer that calls the non-host Cronut
; lexical unit's `is-odd?` function.

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


(require rackunit)

(require #/for-syntax racket/base)

(require #/for-syntax cronut/private/cronut)

(require #/for-syntax #/only-in cronut/private/shim
  shim-require-various)

(begin-for-syntax #/shim-require-various)

(require #/only-in
  (submod cronut/tests/02-odd-manually
    :%private/generated:cronut:lexical-unit)
  [lexical-unit-compile-time definer])

(require #/only-in cronut/private/shim shim-require-various)

(shim-require-various)

; (We provide nothing from this module.)


(define-syntax-parse-rule (require-definer-host definer-host:id)
  
  #:with host
  (dissect definer
    (module-contents-for-lexical-unit _ (elsewhere-bundle host _))
  #/simplified-module-spine->racket-module-path host)
  
  (require #/only-in
    (submod host :%private/generated:cronut:lexical-unit)
    [lexical-unit-compile-time definer-host]))

(require-definer-host definer-host)

(define-for-syntax entry-for-is-odd?
  (dissect definer-host
    (module-contents-for-lexical-unit _
      (here-bundle _ compiled-lexical-units))
  #/dissect
    (hash-ref compiled-lexical-units
      (make-module-spine 'cronut 'tests '02-odd-manually))
    (compiled-lexical-unit functions)
  #/hash-ref functions 'is-odd?))

(begin-for-syntax #/define-namespace-anchor anchor)

(define-for-syntax transform-is-odd?
  (dissect entry-for-is-odd?
    (compiled-lexical-unit-entry-for-single-argument-function x body)
  #/eval #`(lambda (#,x) #,body)
    (namespace-anchor->namespace anchor)))

(define-syntax-parse-rule (is-odd? x:expr)
  #:with result (transform-is-odd? #'x)
  result)


(check-equal? (is-odd? 5) #t)
(check-equal? (is-odd? 6) #f)
