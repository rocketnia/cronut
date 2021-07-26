#lang parendown racket/base

; cronut/tests/01-call-function-from-racket
;
; A unit test which defines a Cronut lexical unit using Racket code
; which has a function which another Racket module can call.
;
; This module is the Racket consumer that calls the function.

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
  (submod cronut/tests/01-define-function-manually
    :%private/generated:cronut:lexical-unit)
  [lexical-unit-compile-time definer])

(require #/only-in cronut/private/shim shim-require-various)

(shim-require-various)

; (We provide nothing from this module.)


(define-for-syntax entry-for-add-two
  (dissect definer
    (module-contents-for-lexical-unit _
      (here-bundle _ compiled-lexical-units))
  #/dissect
    (hash-ref compiled-lexical-units
      (make-module-spine 'cronut 'tests '01-define-function-manually))
    (compiled-lexical-unit functions)
  #/hash-ref functions 'add-two))

(begin-for-syntax #/define-namespace-anchor anchor)

(define-for-syntax transform-add-two
  (dissect entry-for-add-two
    (compiled-lexical-unit-entry-for-single-argument-function x body)
  #/eval #`(lambda (#,x) #,body)
    (namespace-anchor->namespace anchor)))

(define-syntax-parse-rule (add-two x:expr)
  #:with result (transform-add-two #'x)
  result)


(check-equal? (add-two 5) 7)
