#lang cronut/racket/base

; cronut/tests/sample-module-for-cronut-racket-base
;
; A sample module for `#lang cronut/racket/base`.

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


; TODO: Figure out why Kate's syntax highlighter for Racket files
; prefers for there to be a space after `define` here.
(require (for-syntax (only-in racket/base #%datum define )))

(require (only-in racket/contract/base contract-out))


(provide
  my-value
  (contract-out
    [my-value-2 exact-nonnegative-integer?])
  my-value-3
  (for-syntax my-value-for-syntax))


(define my-value 75)

(example-cronut-declaration
  (define my-value-2 75))

(example-cronut-declaration
  (define my-value-3 75))

(begin-for-syntax
  (define my-value-for-syntax 75))
