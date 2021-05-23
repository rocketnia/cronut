#lang racket/base

; cronut/private/racket-default-reader-module-language.rkt
;
; The expansion-time module language corresponding to the language
; `#lang cronut/racket-default-reader`, which defines a Cronut module
; in a way that reuses Racket's reader rather than performing its own
; text processing.

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

(require (only-in cronut define-cronut-module-here))


(provide (rename-out [-#%module-begin #%module-begin]))


(define-syntax-parse-rule (-#%module-begin decl:expr ...)
  (#%module-begin (define-cronut-module-here decl ...)))
