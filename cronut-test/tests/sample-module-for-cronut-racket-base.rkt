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


(provide
  my-value
  my-value-2
  my-value-3)


(define my-value 75)

(example-cronut-declaration
  (define my-value-2 75))

(example-cronut-declaration
  (define my-value-3 75))
