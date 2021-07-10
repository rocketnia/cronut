#lang parendown racket/base

; cronut/private/shim
;
; Import lists, debugging constants, and other utilities that are
; useful primarily for this codebase.

;   Copyright 2021 The Lathe Authors
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


(require #/for-syntax racket/base)

(require #/for-syntax #/only-in syntax/parse syntax-parse)

(require #/for-syntax #/only-in lathe-comforts fn w-)


(provide
  shim-require-various)


(define-syntax (shim-require-various stx)
  (syntax-protect
  #/syntax-parse stx #/ (_)
  #/w- break (fn id #/datum->syntax stx id)
    #`(require
        
        (only-in #,(break 'racket/contract/base)
          -> and/c any/c contract-name hash/c none/c or/c
          rename-contract)
        (only-in #,(break 'racket/contract/combinator)
          coerce-contract)
        
        (only-in #,(break 'lathe-comforts) w-)
        (only-in #,(break 'lathe-comforts/struct)
          auto-equal auto-write define-imitation-simple-generics
          define-imitation-simple-struct)
        (only-in #,(break 'lathe-comforts/contract) fix/c)
        (only-in #,(break 'lathe-comforts/match) match/c)
        
        )))
