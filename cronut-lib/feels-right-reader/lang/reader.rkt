#lang racket/base

; cronut/feels-right-reader/lang/reader
;
; The language `#lang cronut/feels-right-reader`, which implements a
; reader with a kind of macro extensibility that cooperates more
; intentionally with parentheses, auto-indentation, and so on. This
; description is a bit woolly, but we're playing this by ear.

;   Copyright 2022 The Cronut Authors
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


(require (only-in racket/language-info [get-info racket-get-info]))
(require (only-in syntax/strip-context strip-context))


; TODO: For now, this `#lang` is an experiment. Eventually, we should
; rename `#lang cronut/feels-right-reader` to something more concise,
; give it a module semantics that isn't just a placeholder, and write
; documentation for it.

(provide
  (rename-out
    [-read read]
    [-read-syntax read-syntax])
  get-info)


(define (-read in)
  (syntax->datum (-read-syntax #f in)))

(define (-read-syntax src in)
  (define module-level-declarations
    (for/list ([i (in-naturals)])
      (define declaration (read-syntax src in))
      #:break (eof-object? declaration)
      declaration))
  (strip-context
    #`(module
        
        ; NOTE: It apparently doesn't matter what we put here, as long
        ; as it's a symbol.
        anything
        
        cronut/private/racket-base-module-language
        
        (#%module-begin
          #,@module-level-declarations))))

(define (get-info key default-value)
  (define (fallback) (racket-get-info key default-value))
  (case key
    
    ; TODO: See if we need this case. Maybe `racket-lexer` is already
    ; the default.
    [
      (color-lexer)
      (dynamic-require 'syntax-color/default-lexer 'racket-lexer)]
    
    ; TODO: Consider providing behavior for the following other
    ; extension points:
    ;
    ;   configure-runtime
    ;     - Initializing the Racket runtime for executing a
    ;       `#lang cronut/racket/base` module directly or interacting
    ;       with it at a REPL.
    ;
    ;   module-language
    ;     - Is this the right place to look for this key? It's a
    ;       key to the `#:info` specification for
    ;       `#lang syntax/module-reader`, but maybe that's not
    ;       related. Other places in the documentation that talk
    ;       about `'module-language` are referring to a syntax
    ;       property.
    
    [else (fallback)]))
