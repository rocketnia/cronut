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

(require cronut/private/cronut-from-racket)

(import-cronut-single-argument-function
  (make-module-spine 'cronut 'tests '01-define-function-manually)
  'add-two
  add-two)

; (We provide nothing from this module.)


(check-equal? (add-two 5) 7)
