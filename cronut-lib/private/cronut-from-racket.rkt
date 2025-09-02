#lang parendown racket/base

; cronut/private/cronut-from-racket
;
; Miscellaneous implementation details of Cronut for the purpose of
; importing Cronut definitions from Racket modules.

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


(require #/for-syntax racket/base)

(require #/for-syntax cronut/private/cronut)
(require #/for-syntax cronut/private/shim)

(begin-for-syntax #/shim-require-various)

(require cronut/private/shim)

(shim-require-various)


(provide
  import-cronut-single-argument-function
  define-lexical-unit-compile-time)



(define-syntax-parse-rule/autoptic
  (import-cronut-single-argument-function
    spine-expr:expr from-id-expr:expr var:id)
  
  (begin
    
    (define-for-syntax definer-spine spine-expr)
    (define-for-syntax from-id from-id-expr)
    
    (define-syntax-parse-rule/autoptic (require-definer definer:id)
      
      #:with module-path
      (expect (simplify-module-spine definer-spine) (just spine)
        ; TODO: Improve this error message.
        (error "Tried to import from a Cronut lexical unit whose module spine couldn't be converted to a Racket module path")
      #/simplified-module-spine->racket-module-path spine)
      
      (require #/only-in
        (submod module-path :%private/generated:cronut:lexical-unit)
        [lexical-unit-compile-time definer]))
    
    (require-definer definer)
    
    (define-syntax (require-definer-host stx)
      (syntax-protect
      #/syntax-parse stx #/ {~autoptic-list (_ definer-host:id)}
      #/dissect definer (module-contents-for-lexical-unit _ bundle)
      #/mat bundle (here-bundle _ _)
        #'(define-for-syntax definer-host definer)
      #/dissect bundle (elsewhere-bundle host _)
        (with-syntax
          (
            [
              module-path
              (simplified-module-spine->racket-module-path host)])
          #'
          (require #/only-in
            (submod module-path
              :%private/generated:cronut:lexical-unit)
            [lexical-unit-compile-time definer-host]))))
    
    (require-definer-host definer-host)
    
    (define-for-syntax compiled
      (dissect definer-host
        (module-contents-for-lexical-unit _
          (here-bundle _ compiled-lexical-units))
      #/hash-ref compiled-lexical-units definer-spine))
    
    (define-for-syntax entry-for-import
      (dissect compiled (compiled-lexical-unit functions)
      ; TODO: Handle the case where there is no function by this name.
      #/hash-ref functions from-id))
    
    (define-for-syntax transform-import
      (dissect entry-for-import
        (compiled-lexical-unit-entry-for-single-argument-function
          x body)
      #/syntax-local-eval #`(lambda (#,x) #,body)))
    
    (define-syntax-parse-rule/autoptic (var x:expr)
      #:with result (transform-import #'x)
      result)
    
    ))



(define-syntax-parse-rule/autoptic
  (define-lexical-unit-compile-time
    lexical-unit-compile-time:id
    own-spine-expr:expr
    known-spines-expr:expr
    own-declared-lexical-unit-expr:expr)
  (begin
    (define-for-syntax own-spine own-spine-expr)
    (define-for-syntax known-spines known-spines-expr)
    (define-for-syntax own-declared-lexical-unit
      own-declared-lexical-unit-expr)
    (define-lexical-unit-compile-time-step-2
      lexical-unit-compile-time
      own-spine
      known-spines
      own-declared-lexical-unit)))

(define-syntax-parse-rule/autoptic
  (define-lexical-unit-compile-time-step-2
    lexical-unit-compile-time:id
    own-spine-expr:expr
    known-spines-expr:expr
    own-declared-lexical-unit-expr:expr)
  
  #:with define-lexical-unit-compile-time-step-3
  (if
    (equal-always? (syntax-local-eval #'own-spine-expr)
      (car #/syntax-local-eval #'known-spines-expr))
    #'define-lexical-unit-compile-time-step-3-as-host
    #'define-lexical-unit-compile-time-step-3-as-guest)
  
  (define-lexical-unit-compile-time-step-3
    lexical-unit-compile-time
    own-spine-expr
    known-spines-expr
    own-declared-lexical-unit-expr))

(define-syntax-parse-rule/autoptic
  (define-lexical-unit-compile-time-step-3-as-guest
    lexical-unit-compile-time:id
    own-spine-expr:expr
    known-spines-expr:expr
    own-declared-lexical-unit-expr:expr)
  (define-for-syntax lexical-unit-compile-time
    (module-contents-for-lexical-unit
      (just-value #/simplify-module-spine own-spine-expr)
      (elsewhere-bundle
        (just-value #/simplify-module-spine #/car known-spines-expr)
        own-declared-lexical-unit-expr))))

(define-syntax-parse-rule/autoptic
  (define-lexical-unit-compile-time-step-3-as-host
    lexical-unit-compile-time:id
    own-spine-expr:expr
    known-spines-expr:expr
    own-declared-lexical-unit-expr:expr)
  
  #:with (known-spine ...)
  (generate-temporaries #/syntax-local-eval #'known-spines-expr)
  
  #:with (known-declared-lexical-unit ...)
  (generate-temporaries #'(known-spine ...))
  
  #:with (own-spine other-known-spine ...) #'(known-spine ...)
  
  #:with
  (own-declared-lexical-unit other-known-declared-lexical-unit ...)
  #'(known-declared-lexical-unit ...)
  
  #:with (other-definer ...)
  (generate-temporaries #'(other-known-spine ...))
  
  
  #:with (known-declaration ...)
  (generate-temporaries #'(known-spine ...))
  
  #:with (known-introduce ...)
  (generate-temporaries #'(known-spine ...))
  
  #:with (known-imports ...)
  (generate-temporaries #'(known-spine ...))
  
  #:with (known-locals ...)
  (generate-temporaries #'(known-spine ...))
  
  #:with (known-compiled-expr ...)
  (generate-temporaries #'(known-spine ...))
  
  #:with (known-run-time-declaration ...)
  (generate-temporaries #'(known-spine ...))
  
  #:with (known-run-compiled-expr ...)
  (generate-temporaries #'(known-spine ...))
  
  #:with (known-compiled ...)
  (generate-temporaries #'(known-spine ...))
  
  #:with (known-run-run-time-declaration ...)
  (generate-temporaries #'(known-spine ...))
  
  
  (begin
    (begin-for-syntax #/match-define (list known-spine ...)
      known-spines-expr)
    
    (define-syntax-parse-rule/autoptic
      (require-definer definer:id definer-spine:expr)
      
      #:with module-path
      (expect
        (simplify-module-spine #/syntax-local-eval #'definer-spine)
        (just spine)
        ; TODO: Improve this error message.
        (error "Tried to import from a Cronut lexical unit whose module spine couldn't be converted to a Racket module path")
      #/simplified-module-spine->racket-module-path spine)
      
      (require #/only-in
        (submod module-path :%private/generated:cronut:lexical-unit)
        [lexical-unit-compile-time definer]))
    
    (define-for-syntax own-declared-lexical-unit
      own-declared-lexical-unit-expr)
    (begin
      (require-definer other-definer other-known-spine)
      (define-for-syntax other-known-declared-lexical-unit
        (dissect other-definer
          (module-contents-for-lexical-unit _
            (elsewhere-bundle _ other-known-declared-lexical-unit))
          other-known-declared-lexical-unit)))
    ...
    
    
    (begin
      (define-for-syntax known-declaration
        (dissect known-declared-lexical-unit
          (declared-lexical-unit _ #/list declaration-syntax)
        #/dissect (syntax->datum declaration-syntax)
          `(declare-using-racket ,get-declaration)
        #/get-declaration))
      
      (define-for-syntax known-introduce (make-syntax-introducer))
      (define-for-syntax known-imports
        (dissect known-declaration
          (cronut-declaration _ imports _ _ _)
        #/list-map imports #/dissectfn (list import _ _ _)
          (known-introduce import)))
      (define-for-syntax known-locals
        (dissect known-declaration (cronut-declaration _ _ locals _ _)
        #/map known-introduce locals))
      
      (... #/define-for-syntax known-compiled-expr
        (dissect known-declaration
          (cronut-declaration _ imports locals build-compiled _)
        #/w- imports
          (list-map imports #/dissectfn (list import _ _ _) import)
        #/with-syntax ([(imports ...) imports] [(locals ...) locals])
        #/apply
          (syntax-local-eval
            #`(lambda (imports ... locals ...) #,build-compiled))
          (append known-imports known-locals)))
      
      (... #/define-for-syntax known-run-time-declaration
        (dissect known-declaration
          (cronut-declaration
            _ imports locals _ build-run-time-declaration)
        #/w- imports
          (list-map imports #/dissectfn (list import _ _ _) import)
        #/with-syntax ([(imports ...) imports] [(locals ...) locals])
        #/apply
          (syntax-local-eval
            #`
            (lambda (imports ... locals ...)
              #,build-run-time-declaration))
          (append known-imports known-locals)))
      
      (define-syntax-parse-rule/autoptic
        (known-run-compiled-expr compiled:id)
        #:with result (syntax-local-introduce known-compiled-expr)
        (define-for-syntax compiled result))
      
      (known-run-compiled-expr known-compiled)
      
      (define-syntax-parse-rule/autoptic
        (known-run-run-time-declaration)
        
        #:with result
        (syntax-local-introduce known-run-time-declaration)
        
        result))
    ...
    
    
    ; TODO: This shares some implementation details with
    ; `import-cronut-single-argument-function`. Factor these out.
    (define-syntax-parse-rule/autoptic
      (resolve-single-argument-function
        var:id compiled-expr:expr from-id-expr:expr)
      (begin
        (define-for-syntax compiled compiled-expr)
        (define-for-syntax from-id from-id-expr)
        
        (define-for-syntax entry-for-import
          (dissect compiled (compiled-lexical-unit functions)
          ; TODO: Handle the case where there is no function by this
          ; name.
          #/hash-ref functions from-id))
        
        (define-for-syntax transform-import
          (dissect entry-for-import
            (compiled-lexical-unit-entry-for-single-argument-function
              x body)
          #/syntax-local-eval #`(lambda (#,x) #,body)))
        
        (define-syntax-parse-rule/autoptic (var x:expr)
          #:with result (transform-import #'x)
          result)))
    
    
    (define-for-syntax compiled-id-hash
      (hash (~@ known-spine #'known-compiled) ...))
    
    
    (... #/define-syntax (resolve-imports stx)
      (syntax-protect
      #/syntax-parse stx #/
        {~autoptic-list (_ to-ids:expr declaration:expr)}
      #/w- to-ids (syntax-local-eval #'to-ids)
      #/dissect (syntax-local-eval #'declaration)
        (cronut-declaration _ imports _ _ _)
      #/with-syntax
        (
          [(to-id ...) (map syntax-local-introduce to-ids)]
          [
            (compiled-expr ...)
            (list-map imports
              (dissectfn (list _ 'single-argument-function spine _)
                (hash-ref compiled-id-hash spine)))]
          [
            (from-id ...)
            (list-map imports
              (dissectfn (list _ 'single-argument-function _ from-id)
                from-id))])
        #'
        (begin
          (resolve-single-argument-function
            to-id compiled-expr 'from-id)
          ...)))
    
    
    (resolve-imports known-imports known-declaration)
    ...
    
    
    (define-for-syntax lexical-unit-compile-time
      (module-contents-for-lexical-unit
        (just-value #/simplify-module-spine own-spine)
        (here-bundle
          (hash
            (~@
              (just-value #/simplify-module-spine known-spine)
              known-declared-lexical-unit)
            ...)
          (hash (~@ known-spine known-compiled) ...))))
    
    (known-run-run-time-declaration)
    ...
    
    ))
