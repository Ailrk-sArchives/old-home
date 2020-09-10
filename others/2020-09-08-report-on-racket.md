-- tag paper racket macros
-- title Report on `Racket`
-- date 2020-09-07
-- source me
;;
# Report on racket

```lisp
#lang typed/racket
(require (for-syntax racket/match))
(require (for-syntax racket/syntax))

; --------------------------------------------
; Report on A quick overview of racket
; This report is not based on one particular paper, but rather a conclusion of
; multiple sources.
; --------------------------------------------

; --------------------------------------------
; Basics
; --------------------------------------------
; Boolean & conditions
; Use square bracket is more friendly to color blind people than
; rainbow bracketes :3
(: condition : Integer -> (U Symbol Void)) ; U is Sum type
(define (condition n)
  (if (< n 50)
    (if (= n 20) 'eq20
      (cond [(= n 30) 'eq30]  ; idiomatic scheme use [] to differentiate
            [(= n 40) 'eq40]  ; bracket.
            [(or (= n 44) (= n 45)) 'eq44or45]
            [else (case n
                    [(1) 'eq1]
                    [(2) 'eq2]
                    [(3) 'eq3]
                    [else (match n  ; you also have pattern matching
                            [4 'eq4]
                            [5 'eq5]
                            [_ (when (= n 9) 'eq9)])])]))
    'gt50)) ; Symbol

; --------------------------------------------
; list
; --------------------------------------------
(: list-fun (Listof (U Symbol Number (Listof Any))))
(define list-fun
  ((lambda ()
     (let* ([x 45]   ; binding in racket
           [y 55]
           [list1 (list 'a x y)] ; with list constructor
           [list2 '(x y)]  ; make datum with quote
           [listss '((1 2 3) (a b c))]) ; naming convention. s for nested level
       (append listss list2 list1)))))

; --------------------------------------------
; quasiquote
; --------------------------------------------
; You have list and quote. between this two is quasiquote
; you can make a quasiquote with (`) and unquote a value with (,) prefix
; In this case you inject x=10 into the quasiquote
(: quasi (->* (#:size Integer) (Listof Number)))
(define (quasi #:size n)
  (: stack (-> Integer (Listof Integer)))
  (define (stack n)
    (case n
      [(<= n 0) '()]
      [else (cons n (stack (- n 1)))]))
  (let ([x 10]
        [xs (list 9 9 9)]
        [ys (stack n)])
    `(1 2 ,x ,@xs ,@ys 3)))  ; @ spread list into the quote (unquote splicing)

; --------------------------------------------
; Module, contract and mutation
; --------------------------------------------
; You can define multiple modules in the same file.
; it's like restricted namespace, you don't define same module in
; different files.
; Modules are all lazily evaluated.
(module bank-account racket ; we are not using typed racket so can use contract
  (provide (contract-out    ; contract impose constraint. like assert.
           [deposit (-> positive? any)]
           [balance (-> positive?)]))
  (define amount 0)
  (define (deposit a) (set! amount (+ amount a)))   ; mutation with set!
  (define (balance) amount))
; the need of contract can be eliminated with proper types
; this module shows how to require untyped module in typed racket.
(module use-bank-account typed/racket
  (provide get-money)
  (require/typed 'bank-account
                 [deposit (-> Positive-Integer Void)])
  (: get-money (-> Void))
  (define (get-money)
    (deposit 5))
  )

; --------------------------------------------
; Macros
; --------------------------------------------
; Make dsl easy!
; Because lisp syntax is so easy, the code your macro expand to is
; just lisp code itself! (instaed of string like in c)
; Some examples of macros

; ** hygenic macro **
; The gloal is that your macros will not have name conlision with macros
; defined by someone else even when they have the same identifier.
; To achieve this, when expanding the macro racket macro expender will
; "color" the syntax object with the scope they live in. With "colors" one
; can ditermine the identity of identifiers.

; Basics ;
; A ** syntax transformer ** is a function takes syntax and return syntax.
; This is an example that ignore the input syntax and transform a string.
; the synax is the constructor of syntax object, and you can write the
; short hand version #'
(define-syntax foo
  (lambda (stx) (syntax "I am foo")))

; Another way Just to show it's the same as function
(define-syntax (say-hi stx)
  (print stx)   ; print the syntax object (the code.)
  #'(displayln "\nhi"))

; you can define your own syntax
(: stx-1 (Syntaxof Any))
(define stx-1 #'(if x (list "true") #f))

; Some eliminator of syntax type.
(: syntax-stat (-> (Syntaxof Any) Void))
(define (syntax-stat stx)
  (display "Source: ") (displayln (syntax-source stx))
  (display "Line: ") (displayln (syntax-line stx))
  (display "Column: ") (display (syntax-column stx)))

; You can convert syntax to other stuffs
(: syntax-convert (-> (Syntaxof Any) (Values Any Any)))
(define (syntax-convert stx)
  (values (syntax->datum stx) (syntax-e stx)))

; ** datum ** is the type to represent s-expression.
; cdr is to get rid of 'reverse-me in the syntax
; ps: with datum->syntax you can even make syntax object on the fly (
; make syntax object that doesn't exist in the argument stx).
; The reason why we need datum->syntax is to color the synatx object so
; so macro system is hygenic.
(define-syntax (reverse-me stx)
  (datum->syntax stx (reverse (cdr (syntax->datum stx)))))
(: reverse-me-driver (Listof Symbol))
; this will transform the syntax into (list 'v 'a "1")
(define reverse-me-driver (reverse-me 'x 'a 'v list))

; if macro example ;
; Classic (if) example. This in haskell can be achieve by function
; because of the lazy evaluation, but because racket evaluate eagerly,
; you need macro to avoid evaluate all parameters together.
(define-syntax (new-if stx)
  (define xs (syntax->list stx))
  (datum->syntax stx `(cond [,(cadr xs) ,(caddr xs)]
                            [else ,(cadddr xs)])))

; new-if version 2 with match. macro code run at compile time, and
; there is only racket/base available. To use other functionalities
; you need to require with ** for-syntax **. For instance, here we imported
; pattern matching.
(define-syntax (new-if2 stx)
  (match (syntax->list stx)
    [(list _ condition true-expr false-expr)
     (datum->syntax stx `(cond [,condition ,true-expr]
                               [else ,false-expr]))]))

; new-if version 3 with syntax case
; syntax-case simplified using pattern matching.
; Some differences:
; 1. you don't need to manually pattern match
; 2. you don't need to feed data into quasi quote any more.
(define-syntax (new-if3 stx)
  (syntax-case stx ()
    [(_ condition true-expr false-expr)
     #'(cond [condition true-expr] [else false-expr])]))

; define-syntax-rule is a short hand for pattern in new-if3
; You can just write code in the body, and they will transform into
; syntax automatically.
(define-syntax-rule (new-if4 condition true-expr false-expr)
  (cond [condition true-expr] [else false-expr]))

; hyphen macro Examples ;
; (hyphen-define a b (args) body) -> (define (a-b args) body)

; ** define-for-syntax ** allows you to define helper functions in
; macro. It's accessible at compile time.
(: stitch-name (-> (Syntaxof Any) (Syntaxof Any) Symbol))
(define-for-syntax (stitch-name s1 s2)
    (string->symbol
      (format "~a-~a"
              (syntax->datum s1)
              (syntax->datum s2))))

; use nested syntax-case.
;   First, syntax part in `syntax-case` is called template. In our case
; the template in the last line is the syntax get outputed.
;   Note parameters of `stitch-name` are syntax #'a #'b rather than pattern
; matched a and b: we made a b template.
;   You can't use a matched pattern outside of a template. But you can make
; an ad hoc template by forcibly using syntax of the symbol of the varible.
; (use #'a for a, etc)
(define-syntax (hyphen-define/1 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
     (syntax-case (datum->syntax
                    #'a   ;
                    (stitch-name #'a #'b))
       ()
       [name #'(define (name args ...)  body0 body ...)])])) ; template.

; ** with-syntax ** is like syntax-case but chagne the order of parameters.
(define-syntax (hyphen-define/2 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
     (with-syntax ([name (datum->syntax #'a (stitch-name #'a #'b))])
       #'(define (name args ...) body0 body ...))]))

; ** with-syntax* ** is the self referential version.
(define-syntax (foo stx)
  (syntax-case stx ()
    [(_ a)
     (with-syntax* ([b #'a]   ; force to use pattern a in template
                    [c #'b])
                   #'c)]))

; short-circuiting or
; evaluate a first, stop the evaluation if it's true.
; If you want to implement or with function you must evaluate both a and b,
; which is not really what you're intended.
(define-syntax-rule (or-short-circuit a b) (if a a b))

; a while loop macro ;
(define-syntax-rule (while condition body ...)
  (let loop ()
    (when condition
      body ...
      (loop))))

; 3. a macro for swap two variables. ;
; ! is convention for mutation
(define-syntax-rule (swap! x y) ; macro are hygienic so no name colisions.
  (let ([tmp x])
    (set! x y)
    (set! y tmp)))
; actual macro

; --------------------------------------------
; Types
; --------------------------------------------
; Typed tree
(define-type Tree (U leaf node))
(struct leaf ([val : Number]))
(struct node ([left : Tree] [right : Tree]))

(: tree-height (-> Tree Integer))
(define (tree-height t)
  (cond [(leaf? t) 1]
        [else (max (+ 1 (tree-height (node-left t)))
                   (+ 1 (tree-height (node-right t))))]))

(: tree-sum (-> Tree Number))
(define (tree-sum t)
  (cond [(leaf? t) (leaf-val t)]
        [else (+ (tree-sum (node-left t))
                 (tree-sum (node-right t)))]))

; Typed maybe
(struct None ())
(struct (a) Some ([v : a]))
(define-type (Opt a) (U None (Some a)))

(: find (-> Number (Listof Number) (Opt Number)))
(define (find v l)
  (cond [(null? l) (None)]
        [(= v (car l)) (Some v)]
        [else (find v (cdr l))]))

; polymorphic functions
; All is the quantifier.
(: list-len (All (a) (-> (Listof a) Integer)))
(define (list-len l)
  (if (null? l)
    0
    (add1 (list-len (cdr l)))))

; varadic
(: sumNumbers (-> Number * Number))
(define (sumNumbers . xs)
  (if (null? xs) 0 (+ (car xs) (apply sumNumbers (cdr xs)))))

; --------------------------------------------
; Exceptions
; --------------------------------------------
; racket also has exception system.
; try catch style. Haskell has similar idiom to
; handle context.
(with-handler ([exn:fail? (lambda (exn) 999)])
              (+ 1 "2"))

; handle case when you break while the program is sleeping.
(with-handler ([exn:break? (lambda (exn) "No time")])
              (sleep 3)
              "phew")

; it will catch numeric value get raised, and return it's
; identity
(with-handler ([number? identity])
              (+ 1 (raise 2)))

; --------------------------------------------
; Some common data structures
; --------------------------------------------

; define structs
; struct is a macro, it will automatically generate methods like
; dog? and dog-* (dog-name, dog-breed) for struct dog.
; it's really like haskell record type, even the accessor part.
(struct dog ([name : String] [breed : String] [age : Positive-Integer]))
; hypotheical way of using struct

; ** string-append ** is ad hoc concat for a list of string
; ** ~a ** is
(: mk-dog-string (-> dog String))
(define (mk-dog-string d)
  (let* ([name (dog-name d)]
         [name-tag (~a (char-upcase (string-ref name 0)) name)])
    (string-append "Dog name: " name-tag " | "
                   "Dog breed: " (dog-breed d) " | "
                   "Dog age: " (~a (dog-age d) " Years old"))))

; To make a mutable struct you need to specify it in the constructor.
(struct rgba ([r : Integer]
              [g : Integer]
              [b : Integer]
              [a : Integer]) #:mutable #:transparent)

; there are some boilerplates that we can potentially eliminate with macro.
(: color-complement! (-> rgba Void))
(define (color-complement! color)
  (: flip (-> Integer Integer))
  (define (flip n)
    (cond [(> n 255) 0]
          [else (- 255 n)]))
    (set-rgba-r! color (flip (rgba-r color)))
    (set-rgba-g! color (flip (rgba-g color)))
    (set-rgba-b! color (flip (rgba-b color)))
    (set-rgba-a! color (flip (rgba-a color))))

; you also have tings like vector.
; typed racket trips here. I can't use any type other than
; Vectorof Integer (even Vectorof Any) ...
(: vector-playground (Vectorof Integer))
(define vector-playground
  (let* ([xs (take '(1 2 3 4 2) 3)]
         [vs-1 (list->vector xs)]
         [vs-2 #(8 8 9 9)]
         [vs-3 (vector-append vs-1 vs-2)])
    vs-3))

; and set $ hash as normal
(: power-set (All (a) (-> (Setof a) (Setof (Setof a)))))
(define (power-set xs)
  (if (= 0 (set-count xs))
    (let ([set1 (inst set a)]
          [set2 (inst set (Setof a))])
      (set2 (set1)))
    (let ([xs- (power-set (set-rest xs))])
      (set-union
        (for/set ([e xs-]) (set-add e (set-first xs)))
        xs-))))

; Also I found this very convinent digraph for data structures.
; DATA STRUCTURE   ACCESS       NUMBER     INDICES
; List:            sequential   Variable   not used
; Struct:          random       Fixed      names
; Vector:          random       Fixed      integer
; Growable vector: random       Variable   integer
; Hash:            random       Variable   hashable
; Splay:           random       Variable   non-integer, total order
```
