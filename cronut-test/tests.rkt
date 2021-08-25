#lang racket/base

; cronut/tests
;
; Unit tests.

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


(require (only-in rackunit check-equal?))

(require
  (only-in cronut/tests/sample-module-for-cronut-racket-default-reader
    hello))
(require
  (only-in cronut/tests/sample-module-for-cronut-racket-base
    my-value my-value-2 my-value-3))

; (We provide nothing from this module.)


; TODO: Write more unit tests.

(check-equal? hello 75)

(check-equal? my-value 75)
(check-equal? my-value-2 75)
(check-equal? my-value-3 75)
