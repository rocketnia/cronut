#lang parendown racket/base

; cronut/private/cronut
;
; Miscellaneous implementation details of the Cronut programming
; language.

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


(require #/only-in cronut/private/shim shim-require-various)

(shim-require-various)


(provide
  
  
  ; Internal representation of Cronut values, patterns, and abstract
  ; interpretation values
  
  name?
  name-impl?
  prop:name
  make-name-impl
  
  other-name?
  other-name-value
  other-name
  
  string-name?
  string-name-value
  string-name
  
  coined-name?
  coined-name-module-spine
  coined-name-name
  coined-name
  
  construction-name?
  construction-name-op-call
  construction-name
  
  name/c
  
  module-spine?
  module-spine-impl?
  prop:module-spine
  make-module-spine-impl
  
  main-module-spine?
  main-module-spine-collection
  main-module-spine-op-call
  main-module-spine
  
  submodule-spine?
  submodule-spine-parent
  submodule-spine-op-call
  submodule-spine
  
  module-spine/c
  
  module-collection?
  module-collection-impl?
  prop:module-collection
  make-module-collection-impl
  
  nil-module-collection?
  nil-module-collection
  
  snoc-module-collection?
  snoc-module-collection-parent
  snoc-module-collection-op-call
  snoc-module-collection
  
  module-collection/c
  
  op-call?
  op-call-op
  op-call-args
  op-call
  
  op-call/c
  
  
  ; Internal representation of Racket modules for Cronut lexical units
  
  module-contents-for-lexical-unit?
  module-contents-for-lexical-unit-declared-racket-module-path
  module-contents-for-lexical-unit-bundle
  module-contents-for-lexical-unit
  
  module-contents-for-lexical-unit/c
  
  module-bundle?
  module-bundle-impl?
  prop:module-bundle
  make-module-bundle-impl
  
  elsewhere-bundle?
  elsewhere-bundle-racket-module-path
  elsewhere-bundle-declared-lexical-unit
  elsewhere-bundle
  
  here-bundle?
  here-bundle-uncompiled-modules
  here-bundle-compiled-modules
  here-bundle
  
  module-bundle/c
  
  declared-lexical-unit?
  declared-lexical-unit-formal-optional-arguments
  declared-lexical-unit-body
  declared-lexical-unit
  
  declared-lexical-unit/c
  
  compiled-lexical-unit-entry-for-single-argument-function?
  compiled-lexical-unit-entry-for-single-argument-function-arg
  compiled-lexical-unit-entry-for-single-argument-function-body-template
  compiled-lexical-unit-entry-for-single-argument-function
  
  compiled-lexical-unit-entry-for-single-argument-function/c
  compiled-lexical-unit?
  compiled-lexical-unit-functions
  compiled-lexical-unit
  
  compiled-lexical-unit/c
  
  )

; TODO: Consider organizing the various implementation details in this
; file into other files someday.



; ===== Miscellaneous ================================================
;
; TODO: See if these should be added to Lathe Comforts or something.

(define (interned-symbol? v)
  (and (symbol? v) (symbol-interned? v)))



; ====================================================================
; Internal representation of Cronut values, patterns, and abstract
; interpretation values
; ====================================================================

; A Cronut value is represented by a `(name/c none/c)` value. (In the
; Cronut MVP, all values can be checked for equality and used as keys,
; so they're all "names" in some sense. Perhaps we should call names
; "values," but for now, we're still in the process of implementing
; Cronut and haven't needed to make a distinction or a change.)
;
; Applying `name/c` to some other contract, such as a contract that
; represents symbolic placeholders for values, can represent values
; that are allowed to have those symbolic placeholders somewhere
; inside. In particular, we expect to use this for pattern-matching
; syntax, where the symbolic placeholders represent variable names
; that will be bound by the pattern.


(define-imitation-simple-generics
  name? name-impl? prop:name make-name-impl 'name 'name-impl (list))

(define-imitation-simple-struct
  (other-name? other-name-value)
  other-name
  'other-name (current-inspector) (auto-write) (auto-equal)
  (#:prop prop:name (make-name-impl)))

(define-imitation-simple-struct
  (string-name? string-name-value)
  string-name
  'string-name (current-inspector) (auto-write) (auto-equal)
  (#:prop prop:name (make-name-impl)))

(define-imitation-simple-struct
  (coined-name? coined-name-module-spine coined-name-name)
  coined-name
  'coined-name (current-inspector) (auto-write) (auto-equal)
  (#:prop prop:name (make-name-impl)))

(define-imitation-simple-struct
  (construction-name? construction-name-op-call)
  construction-name
  'construction-name (current-inspector) (auto-write) (auto-equal)
  (#:prop prop:name (make-name-impl)))

(define (name/c other-name-value/c)
  (w- other-name-value/c (coerce-contract 'name/c other-name-value/c)
  #/rename-contract
    (fix/c fixed-name/c
      (and/c name?
        (or/c
          (match/c other-name other-name-value/c)
          (match/c string-name (and/c symbol? symbol-interned?))
          (match/c coined-name
            (module-spine/c other-name-value/c)
            fixed-name/c)
          (match/c construction-name
            (op-call/c other-name-value/c)))))
    `(name/c ,(contract-name other-name-value/c))))


(define-imitation-simple-generics
  module-spine?
  module-spine-impl?
  prop:module-spine
  make-module-spine-impl
  'module-spine 'module-spine-impl (list))

(define-imitation-simple-struct
  (main-module-spine?
    main-module-spine-collection
    main-module-spine-op-call)
  main-module-spine
  'main-module-spine (current-inspector) (auto-write) (auto-equal)
  (#:prop prop:module-spine (make-module-spine-impl)))

(define-imitation-simple-struct
  (submodule-spine? submodule-spine-parent submodule-spine-op-call)
  submodule-spine
  'submodule-spine (current-inspector) (auto-write) (auto-equal)
  (#:prop prop:module-spine (make-module-spine-impl)))

(define (module-spine/c other-name-value/c)
  (w- other-name-value/c
    (coerce-contract 'module-spine/c other-name-value/c)
  #/rename-contract
    (fix/c fixed-module-spine/c
      (and/c module-spine?
        (or/c
          (match/c main-module-spine
            (module-collection/c other-name-value/c)
            (op-call/c other-name-value/c))
          (match/c submodule-spine
            fixed-module-spine/c
            (op-call/c other-name-value/c)))))
    `(module-spine/c ,(contract-name other-name-value/c))))


(define-imitation-simple-generics
  module-collection?
  module-collection-impl?
  prop:module-collection
  make-module-collection-impl
  'module-collection 'module-collection-impl (list))

(define-imitation-simple-struct
  (nil-module-collection?)
  nil-module-collection
  'nil-module-collection (current-inspector) (auto-write) (auto-equal)
  (#:prop prop:module-collection (make-module-collection-impl)))

(define-imitation-simple-struct
  (snoc-module-collection?
    snoc-module-collection-parent
    snoc-module-collection-op-call)
  snoc-module-collection
  'snoc-module-collection (current-inspector) (auto-write) (auto-equal)
  (#:prop prop:module-collection (make-module-collection-impl)))

(define (module-collection/c other-name-value/c)
  (w- other-name-value/c
    (coerce-contract 'module-collection/c other-name-value/c)
  #/rename-contract
    (fix/c fixed-module-collection/c
      (and/c module-collection?
        (or/c
          (match/c nil-module-collection)
          (match/c snoc-module-collection
            fixed-module-collection/c
            (op-call/c other-name-value/c)))))
    `(module-collection/c ,(contract-name other-name-value/c))))


(define-imitation-simple-struct
  (op-call? op-call-op op-call-args)
  op-call
  'op-call (current-inspector) (auto-write) (auto-equal))

(define (op-call/c other-name-value/c)
  (w- other-name-value/c
    (coerce-contract 'op-call/c other-name-value/c)
  #/rename-contract
    (match/c op-call
      (name/c other-name-value/c)
      (hash/c (name/c none/c) (name/c other-name-value/c)))
    `(module-collection/c ,(contract-name other-name-value/c))))



; ====================================================================
; Internal representation of Racket modules for Cronut lexical units
; ====================================================================


(define-imitation-simple-struct
  (module-contents-for-lexical-unit?
    module-contents-for-lexical-unit-declared-racket-module-path
    module-contents-for-lexical-unit-bundle)
  module-contents-for-lexical-unit
  'module-contents-for-lexical-unit (current-inspector)
  (auto-write)
  (auto-equal))

(define (module-contents-for-lexical-unit/c)
  (rename-contract
    (match/c module-contents-for-lexical-unit
      ; NOTE: If a Racket module represents a Cronut lexical unit
      ; using `module-contents-for-lexical-unit`, the given Racket
      ; module path should refer to the same module.
      module-path?
      (module-bundle/c))
    `(module-contents-for-lexical-unit/c)))


(define-imitation-simple-generics
  module-bundle?
  module-bundle-impl?
  prop:module-bundle
  make-module-bundle-impl
  'module-bundle 'module-bundle-impl (list))

(define-imitation-simple-struct
  (elsewhere-bundle?
    elsewhere-bundle-racket-module-path
    elsewhere-bundle-declared-lexical-unit)
  elsewhere-bundle
  'elsewhere-bundle (current-inspector) (auto-write) (auto-equal)
  (#:prop prop:module-bundle (make-module-bundle-impl)))

(define-imitation-simple-struct
  (here-bundle?
    here-bundle-uncompiled-modules
    here-bundle-compiled-modules)
  here-bundle
  'here-bundle (current-inspector) (auto-write) (auto-equal)
  (#:prop prop:module-bundle (make-module-bundle-impl)))

(define (module-bundle/c)
  (rename-contract
    (and/c module-bundle?
      (or/c
        
        ; NOTE: If one Racket module represents a Cronut lexical unit
        ; using `elsewhere-bundle`, the given Racket module path
        ; should refer to another Racket module that represents a
        ; Cronut lexical unit using `here-bundle` and that has a
        ; declared lexical unit whose Racket module path refers back
        ; to the first Racket module. Conversely, if one Racket module
        ; represents a Cronut lexical unit using `here-bundle`, one of
        ; its declared lexical units' Racket module paths should refer
        ; to the same Racket module, and each of the other declared
        ; lexical units' Racket module paths should refer to another
        ; Racket module that represents a Cronut lexical unit using an
        ; `elsewhere-bundle` where the given Racket module path refers
        ; back to the first Racket module.
        
        ; NOTE: For each Racket module path with a declared lexical
        ; unit, there should also be a compiled lexical unit for the
        ; corresponding module spine that passes no arguments anywhere
        ; along that Racket module path. Conversely, for each compiled
        ; lexical unit, there should be a declared lexical unit at the
        ; Racket module path corresponding to that module spine with
        ; all its arguments stripped away.
        
        (match/c elsewhere-bundle
          module-path?
          (declared-lexical-unit/c))
        (match/c here-bundle
          (hash/c module-path? (declared-lexical-unit/c))
          (hash/c (module-spine/c none/c)
            ; TODO: What about run-time arguments to the compiled
            ; lexical units? Right now, module spines only seem to
            ; account for compile-time information.
            (compiled-lexical-unit/c)))))
    `(module-bundle/c)))


(define-imitation-simple-struct
  
  (declared-lexical-unit?
    
    ; A set of the module/collection's optional arguments.
    declared-lexical-unit-formal-optional-arguments
    
    ; A bunch of syntax objects that are ready to be compiled once
    ; those arguments are supplied.
    declared-lexical-unit-body)
  
  declared-lexical-unit
  'declared-lexical-unit (current-inspector)
  (auto-write)
  (auto-equal))

(define (declared-lexical-unit/c)
  (rename-contract
    (match/c declared-lexical-unit (set/c keyword?) (listof syntax?))
    `(declared-lexical-unit/c)))


(define-imitation-simple-struct
  (compiled-lexical-unit-entry-for-single-argument-function?
    compiled-lexical-unit-entry-for-single-argument-function-arg
    compiled-lexical-unit-entry-for-single-argument-function-body-template)
  compiled-lexical-unit-entry-for-single-argument-function
  'compiled-lexical-unit-entry-for-single-argument-function
  (current-inspector)
  (auto-write)
  (auto-equal))

(define (compiled-lexical-unit-entry-for-single-argument-function/c)
  (rename-contract
    (match/c compiled-lexical-unit-entry-for-single-argument-function
      identifier?
      
      ; NOTE: The body template should be an expression, and its only
      ; free variable should be the argument identifier. If the value
      ; of the argument variable is a syntax object representing an
      ; expression with any number of free variables, then the result
      ; of the body template should be a syntax object representing an
      ; expression with the same free variables that calls the
      ; function with that argument expression.
      syntax?)
    `(compiled-lexical-unit-entry-for-single-argument-function/c)))


; TODO: Flesh this out with more kinds of compiled module information.
; Let's see.... We should have a set of submodules/resident lexical
; units; a set of coined names; a set of defined constructors; a set
; of defined interfaces; some kind of information about specialization
; matrices; a set of defined compile-time values; and some kind of
; information about imports and exports. For each specialization or
; other quinable expression, we should also have that expression's
; compilation to Racket. Currently we have single-argument functions
; and make no effort to make them quinable, but eventually they should
; be expressible in terms of constructors that implement a function
; interface.
;
(define-imitation-simple-struct
  
  (compiled-lexical-unit?
    
    ; A map of the defined single-argument functions.
    compiled-lexical-unit-functions)
  
  compiled-lexical-unit
  'compiled-lexical-unit (current-inspector)
  (auto-write)
  (auto-equal))

(define (compiled-lexical-unit/c)
  (rename-contract
    (match/c compiled-lexical-unit
      (hash/c interned-symbol?
        (compiled-lexical-unit-entry-for-single-argument-function/c)))
    `(compiled-lexical-unit/c)))
