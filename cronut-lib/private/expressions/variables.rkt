#lang racket/base

; cronut/private/expressions/variables.rkt
;
; Cronut expressions having to do with accessing and binding local
; variables.

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
;   - The declarations in this module create structure types and add
;     generic interface/structure type property specializations to
;     them. These are the Racket concepts, not the Cronut concepts of
;     constructors and interfaces, even though there's an intentional
;     resemblance. Usually, Racket structure types have to specify the
;     full set of generic interfaces they implement when they're
;     defined. We're going to get around this by actually just quoting
;     all this code and exporting it so that some other module in this
;     codebase can cobble together all the pieces.
;
;   - We're using Racket contracts, kind of. We're preparing to use
;     them. In `define-constructor`, we defining what the contracts
;     *would* be for each constructor, but the constructor may or may
;     not actually be compiled to check those contracts. The
;     `instance/c` combinator makes a flat contract that recognizes
;     instances of an interface. While we don't define any interfaces
;     in the code we've written so far, we assume that each interface
;     definition has a corresponding contract combinator that affects
;     just that interface on the instance, without interfering with
;     any of the instance's other interfaces. Defining these contracts
;     requires cooperation from the interface's methods themselves, so
;     we assume those methods exist. Since we assume they exist, we
;     implement them in our specializations here.
;
;   - The method `(expr) map-recursive-occurrences` is designed to
;     support contracts corresponding to the `(expr)`,
;     `(expr-substitutable)`, and `(expr-interpretable-as-racket)`
;     interfaces. These contracts can use it to traverse through an
;     expression and transform each node so that we're asserting that
;     it obeys these interfaces. The occurrence path (like
;     `occurrence-in-expr-let-binding`) is passed into the
;     transformation function to assist with generating good error
;     messages. The set of bound variables is passed in in case it has
;     any bearing on how a subexpression is transformed.
;
;   - If the `(expr)` interface were an indexed type family, the set
;     of variables in scope for it would be one of its indexes. It's
;     essentially a field belonging to the compile-time information.
;     That's why, at run time, we propagate that information from the
;     value to its consumer (`(expr) get-free-vars`) and from the
;     consumer to the value (`(expr) map-recursive-occurrences`). Note
;     that the set of bound variables known to the consumer may exceed
;     the set of free variables known to the expression; this will
;     likely come up any time a type family index is depended upon in
;     a monotonic way.
;
;   - The interface `(expr-substitutable)` isn't one we foresee
;     needing. However, it does give us the ability to replace an
;     expression's free variables with another set of variables, which
;     lets us understand the content of an expression as being
;     independent of the choice of variables. (TODO: Likewise, someday
;     we may want a way to assert that two expressions are equal up to
;     a consistent substitution, or to zip two expressions in a way
;     that computes a replacement variable name from two that appear
;     in all the same positions.)
;
;   - (TODO: Hmm.... What actually remains the same about a free
;     variable between two expressions that give it different names is
;     the set of locations where it appears. Perhaps we can consider
;     the variables of an expression to have two names: The set of
;     locations, which serves as a type family index, and the symbolic
;     name, which is treated like a collection element. The set of
;     locations is the "key" that maps to the symbolic name "value"
;     (although the symbolic names must also be unique).)
;
;   - (TODO: Let's see if we should have `get-recursive-occurrences`
;     and `replace-recursive-occurrences` instead of
;     `map-recursive-occurrences`. We went with the "map" approach so
;     that potentially some expressions could have subexpressions that
;     are only explored on demand (e.g. to represent infinite numbers
;     of subexpressions or subexpressions that depend on a run time
;     value). However, the operations we want to do with these
;     expressions, like obtaining their free variables and running
;     them through cross-compilers, tend to assume the whole
;     expression is a tree of data that's known up front. So there
;     probably isn't going to be any expression that can't implement
;     the "get" and "replace" approach.)


; TODO: We depend on some things in this module that we should define
; at some point:
;
; define-constructor, field
; specialize, method
; invoke
;
; cronut-any/c
; instance/c
;
; cronut-set-from-list
; cronut-set-add
; cronut-set-union
; cronut-set-union-list
; cronut-set-minus
; cronut-dict-ref
; cronut-dict-set
;
; (NOTE: These are defined in `cronut/private/expressions`.)
; expr, map-recursive-occurrences, get-free-vars
; expr-substitutable, substitute
; expr-interpretable-as-racket, interpret


#|

(define-constructor (expr-var (field name cronut-any/c)))
(specialize (expr) (expr-var name)
  (method (map-recursive-occurrences env transform)
    (expr-var name))
  (method (get-free-vars)
    (cronut-set-from-list (list name))))
(specialize (expr-substitutable) (expr-var name)
  (method (substitute env)
    (cronut-dict-ref env name)))
(specialize (expr-interpretable-as-racket) (expr-var name)
  (method (interpret env)
    (cronut-dict-ref env name)))

(define-constructor
  (expr-let
    (field bindings
      (listof (list/c cronut-any/c (instance/c (expr)))))
    (field body (instance/c (expr)))))
(define-constructor
  (occurrence-in-expr-let-binding
    (field i natural?)
    (field var cronut-any/c)))
(define-constructor (occurrence-in-expr-let-body))
(specialize (expr) (expr-let bindings body)
  (method (map-recursive-occurrences env transform)
    (expr-let
      (for/list ([i (in-naturals)] [binding (in-list bindings)])
        (match-define (list var val) binding)
        (list var
          (transform (occurrence-in-expr-let-binding i var) env val)))
      (transform (occurrence-in-expr-let-body)
        (for/fold ([body-env env]) ([binding (in-list bindings)])
          (match-define (list var val) binding)
          (cronut-set-add body-env var))
        body)))
  (method (get-free-vars)
    (cronut-set-union
      (cronut-set-union-list
        (for/list ([binding (in-list bindings)])
          (match-define (list var val) binding)
          (invoke val (expr) (get-free-vars))))
      (cronut-set-minus
        (invoke body (expr) (get-free-vars))
        (cronut-set-from-list
          (for/list ([binding (in-list bindings)])
            (match-define (list var val) binding)
            var))))))
(specialize (expr-substitutable) (expr-let bindings body)
  (method (substitute env)
    (expr-let
      (for/list ([binding (in-list bindings)])
        (match-define (list var val) binding)
        (list var (invoke val (expr-substitutable) (substitute env))))
      (invoke body (expr-substitutable)
        (substitute
          (for/fold ([body-env env]) ([binding (in-list bindings)])
            (match-define (list var val) binding)
            (cronut-dict-set body-env var (expr-var var))))))))
(specialize (expr-interpretable-as-racket) (expr-let bindings body)
  (method (interpret env)
    (invoke body (expr-interpretable-as-racket)
      (interpret
        (for/fold ([body-env env]) ([binding (in-list bindings)])
          (match-define (list var val) binding)
          (cronut-dict-set body-env var
            (invoke val (expr-interpretable-as-racket)
              (interpret env))))))))

|#
