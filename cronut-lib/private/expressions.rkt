#lang racket/base

; cronut/private/expressions.rkt
;
; Interfaces for Cronut expressions.

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


; TODO: Change the language of this module to
; `cronut/private/implementation`.
;
; TODO: Uncomment this module.

; Some notes about what's going on here:
;
;   - Within a `define-interface`, each `method` and each `argument`
;     of each `method` is associated with three contracts: The full
;     contract that would be used for debugging and documenting this
;     codebase, the partial contract that needs to apply when call
;     sites beyond this codebase invoke the method, and the partial
;     contract that needs to apply when call sites beyond this
;     codebase implement the method.
;
;   - TODO: Implement a contract for `(expr-substitutable)` instances.
;     They should be `(expr)` instances whose
;     `map-recursive-occurrences` produce only other
;     `(expr-substitutable)` instances satisfying the same constraint.
;     To avoid redundant traversals, we should have an
;     `expr-substitutable-confirmed` constructor that verifies the
;     condition only when it's constructed for the first time. That
;     constructor's instances don't themselves need to be instances of
;     `(expr)` or `(expr-substitutable)`.
;
;   - TODO: It seems like `(expr-substitutable) substitute` should
;     require that the value its invoked on satisfy the more complex
;     contract above. Perhaps we just won't invoke it through the
;     method. Similarly, `(expr-interpretable-as-racket) interpret`
;     should impose a condition on its input that's analogous to the
;     `(expr-substitutable)` condition but for
;     `(expr-interpretable-as-racket)` instead.
;
;   - TODO: How should we express that
;     `(expr) map-recursive-occurrences` should be invoked only by
;     code within this codebase, abstraction-breaking code, and
;     error-message-producing code? Ah, perhaps we should designate
;     `(expr-substitutable) substitute` as the same kind of method, so
;     that most code only invokes it through a contract-imposing
;     wrapper function.
;
;   - TODO: Oh, we haven't required that the environment arguments to
;     `(expr-substitutable) substitute` and
;     `(expr-interpretable-as-racket) interpret` bind at least as many
;     entries as what the expressions have in their
;     `(expr) get-free-variables`. Perhaps we should do that by using
;     an `expr-confirmed` constructor that has a precomputed set of
;     free variables so that we don't have to traverse the expression
;     to find its free variables each time we need to check the
;     contract. Perhaps we should also intern that set of free
;     variables and have a `cronut-dict-superset-confirmed`
;     constructor so that the dictionary doesn't have to be traversed
;     every time. Are these contract-confirming wrappers going to
;     proliferate all over the place?


; TODO: We depend on some things in this module that we should define
; at some point:
;
; define-interface, method, argument
;
; cronut-any/c
; instance/c
;
; cronut-set/c
; cronut-dict/c


#|

(define-interface expr
  (method
    (map-recursive-occurrences
      (argument env
        (cronut-set/c cronut-any/c)
        (cronut-set/c cronut-any/c)
        any/c)
      (argument transform
        (->
          cronut-any/c
          (cronut-set/c cronut-any/c)
          (instance/c (expr))
          (instance/c (expr)))
        (-> any/c any/c any/c (instance/c (expr)))
        (->
          cronut-any/c
          (cronut-set/c cronut-any/c)
          (instance/c (expr))
          any)))
    (instance/c (expr))
    any
    (instance/c (expr)))
  (method (get-free-vars)
    (cronut-set/c cronut-any/c)
    any
    (cronut-set/c cronut-any/c)))

(define-interface expr-substitutable
  (method
    (substitute
      (argument env
        (cronut-dict/c cronut-any/c
          (and/c
            (instance/c (expr))
            (instance/c (expr-substitutable))))
        (cronut-dict/c cronut-any/c
          (and/c
            (instance/c (expr))
            (instance/c (expr-substitutable))))
        any/c))
    (and/c
      (instance/c (expr))
      (instance/c (expr-substitutable)))
    any
    (and/c
      (instance/c (expr))
      (instance/c (expr-substitutable)))))

(define-interface expr-interpretable-as-racket
  (method
    (interpret
      (argument env
        (cronut-dict/c cronut-any/c any/c)
        (cronut-dict/c cronut-any/c any/c)
        any/c))
    any/c
    any
    any/c))

|#
