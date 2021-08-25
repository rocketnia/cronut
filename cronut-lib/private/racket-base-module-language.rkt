#lang racket/base

; cronut/private/racket-base-module-language.rkt
;
; The expansion-time module language corresponding to the language
; `#lang cronut/racket/base`, which defines a Racket module that has
; access to most of the bindings of `racket/base` while also having
; the ability to make Cronut declarations for things like cyclic
; module dependencies.

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


(require (for-syntax (only-in syntax/parse expr)))

(require (only-in syntax/parse/define define-syntax-parse-rule))


(provide (except-out (all-from-out racket/base) #%module-begin))
(provide (rename-out [-#%module-begin #%module-begin]))


(define-syntax-parse-rule (-#%module-begin decl:expr ...)
  (#%module-begin decl ...))
