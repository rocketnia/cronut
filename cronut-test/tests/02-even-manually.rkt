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
  
  (require cronut/private/cronut-from-racket)
  
  
  (provide #/for-syntax lexical-unit-compile-time)
  
  
  (define-lexical-unit-compile-time lexical-unit-compile-time
    (make-module-spine 'cronut 'tests '02-even-manually)
    (list
      (make-module-spine 'cronut 'tests '02-even-manually)
      (make-module-spine 'cronut 'tests '02-odd-manually))
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
          (or (zero? x) (#,is-odd? #/sub1 x)))))))
