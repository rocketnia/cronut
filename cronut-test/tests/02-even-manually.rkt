#lang parendown racket/base

; cronut/tests/02-even-manually
;
; A unit test which defines two mutually recursive Cronut lexical
; units using Racket code and calls the function from the non-host
; lexical unit from another Racket module.
;
; This module is the host Cronut lexical unit. It defines the
; `is-even?` function.

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


; (We provide nothing from this module.)


(module :%private/generated:cronut:lexical-unit racket/base
  
  (require #/for-syntax racket/base)
  
  (require #/for-syntax cronut/private/cronut)
  
  (require #/for-syntax #/only-in cronut/private/shim
    shim-require-various)
  
  (begin-for-syntax #/shim-require-various)
  
  
  (provide #/for-syntax lexical-unit-compile-time)
  
  
  (define-for-syntax lexical-unit-compile-time
    (module-contents-for-lexical-unit
      (just-value #/simplify-module-spine #/make-module-spine
        'cronut 'tests '02-even-manually)
      (here-bundle
        ; TODO: Add syntax objects to these empty lists so that these
        ; declared lexical units compile to the compiled versions
        ; below when they're provided with no arguments. Right now, we
        ; haven't built the appropriate compiler or any suitable
        ; syntaxes for it to compile yet.
        (hash
          (just-value #/simplify-module-spine #/make-module-spine
            'cronut 'tests '02-even-manually)
          (declared-lexical-unit (set) (list))
          (just-value #/simplify-module-spine #/make-module-spine
            'cronut 'tests '02-odd-manually)
          (declared-lexical-unit (set) (list)))
        (hash
          (make-module-spine 'cronut 'tests '02-even-manually)
          (compiled-lexical-unit
            (hash 'is-even?
              (compiled-lexical-unit-entry-for-single-argument-function
                #'x
                #'#`(0:is-even? #,x))))
          (make-module-spine 'cronut 'tests '02-odd-manually)
          (compiled-lexical-unit
            (hash 'is-odd?
              (compiled-lexical-unit-entry-for-single-argument-function
                #'x
                #'#`(1:is-odd? #,x))))))))
  
  (define (0:is-even? x)
    (or (zero? x) (1:is-odd? #/sub1 x)))
  
  (define (1:is-odd? x)
    (and (not #/zero? x) (0:is-even? #/sub1 x)))
  
  )
