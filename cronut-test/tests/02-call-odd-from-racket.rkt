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

(require cronut/private/cronut-from-racket)

(import-cronut-single-argument-function
  (make-module-spine 'cronut 'tests '02-odd-manually)
  'is-odd?
  is-odd?)

; (We provide nothing from this module.)


(check-equal? (is-odd? 5) #t)
(check-equal? (is-odd? 6) #f)
