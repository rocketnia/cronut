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

; TODO: Export something from this module. We're putting various
; implementation details in this file that we'll probably want to
; organize into other files someday, but right now even the
; implementation details aren't being used.



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
  (coined-name? coined-name-module-path coined-name-name)
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
            (module-path/c other-name-value/c)
            fixed-name/c)
          (match/c construction-name
            (op-call/c other-name-value/c)))))
    `(name/c ,(contract-name other-name-value/c))))


(define-imitation-simple-generics
  module-path?
  module-path-impl?
  prop:module-path
  make-module-path-impl
  'module-path 'module-path-impl (list))

(define-imitation-simple-struct
  (main-module-path?
    main-module-path-collection
    main-module-path-op-call)
  main-module-path
  'main-module-path (current-inspector) (auto-write) (auto-equal)
  (#:prop prop:module-path (make-module-path-impl)))

(define-imitation-simple-struct
  (submodule-path? submodule-path-parent submodule-path-op-call)
  submodule-path
  'submodule-path (current-inspector) (auto-write) (auto-equal)
  (#:prop prop:module-path (make-module-path-impl)))

(define (module-path/c other-name-value/c)
  (w- other-name-value/c
    (coerce-contract 'module-path/c other-name-value/c)
  #/rename-contract
    (fix/c fixed-module-path/c
      (and/c module-path?
        (or/c
          (match/c main-module-path
            (module-collection/c other-name-value/c)
            (op-call/c other-name-value/c))
          (match/c submodule-path
            fixed-module-path/c
            (op-call/c other-name-value/c)))))
    `(module-path/c ,(contract-name other-name-value/c))))


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

; TODO
