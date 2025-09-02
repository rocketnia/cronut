#lang racket/base

; cronut
;
; A Racket library with entrypoints to the Cronut programming
; language.

;   Copyright 2021, 2025 The Cronut Authors
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

(require (only-in lathe-comforts define-syntax-parse-rule/autoptic))


; TODO: Implement `define-cronut-module-here` in a way that isn't just
; a placeholder, and write documentation for it.
(provide define-cronut-module-here)


(define-syntax-parse-rule/autoptic
  (define-cronut-module-here decl:expr ...)
  (begin
    (define hello (begin (#%datum . decl) ...))
    (provide hello)))
