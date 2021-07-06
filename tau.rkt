#lang racket

(require racket/match)
(require racket/undefined)
(require srfi/1)
(require syntax/parse/define)

(provide (rename-out [-module-begin #%module-begin])
         (except-out (all-from-out racket) #%module-begin))

(current-namespace (make-base-namespace))

(define (typeq? t s)
  (or (member 'any (list t s))
      (equal? t s)
      (and (list? t)
           (list? s)
           (every typeq? t s))))

(define/match (typeof exp types)
  [(`(FUN ,ftype ,args ,@body) _)
   (let* ([atypes (drop-right ftype 1)]
          [types (append (zip args atypes) types)]
          [rtype (last ftype)]
          [btype (for/and ([exp body]) (typeof exp types))])
     (unless (typeq? rtype btype)
       (raise "An invalid return was found"))
     (append atypes (list btype)))]
  [(`(IF ,test ,then ,else) _)
   (let ([testtype (typeof test types)]
         [thentype (typeof then types)]
         [elsetype (typeof else types)])
     (unless (typeq? thentype elsetype)
       (raise "An invalid conditional was found"))
     (if (equal? 'any thentype) elsetype thentype))]
  [(`(SET ,var ,val) _)
   (let ([vartype (typeof var types)]
         [valtype (typeof val types)])
     (unless (typeq? vartype valtype)
       (raise "An invalid assignment was found"))
     vartype)]
  [(`(APP ,fun ,@args) _)
   (let* ([atypes (map (lambda (arg) (typeof arg types)) args)]
          [ftype (typeof fun types)]
          [rtype (last ftype)]
          [etypes (drop-right ftype 1)])
     (unless (typeq? atypes etypes)
       (raise "An invalid application was found"))
     rtype)]
  [((? symbol? s) _)
   (unless (assoc s types)
     (error "An undefined symbol was found" s))
   (second (assoc s types))]
  [((? char?) _) 'char]
  [((? string?) _) 'string]
  [((? number?) _) 'number]
  [((? (lambda (u) (eq? u undefined))) _) 'any]
  [(_ _) (raise "Error")])

(define/match (transform-local exp)
  [(`(let (((,vars ,types) ,vals) ...) ,@body))
   `(APP (FUN (,@types any) ,vars ,@(map transform-local body))
         ,@(map transform-local vals))]
  [(`(letrec (((,vars ,types) ,vals) ...) ,@body))
   (transform-local
    `(let ,(map (lambda (var type) `((,var ,type) ,undefined)) vars types)
       ,@(map (lambda (var val) `(set! ,var ,val)) vars vals)
       ,@body))]
  [(`(set! ,var ,val))
   `(SET ,var ,(transform-local val))]
  [(`(lambda ,ftype ,args ,@body))
   `(FUN ,ftype ,args ,@(map transform-local body))]
  [(`(if ,test ,then ,else))
   `(IF ,(transform-local test)
        ,(transform-local then)
        ,(transform-local else))]
  [(`(,fun ,@args))
   `(APP ,(transform-local fun) ,@(map transform-local args))]
  [(_) exp])

(define/match (transform-global exps)
  [(`((define (,var ,type) ,val) ,@rest))
   `((letrec (((,var ,type) ,val)) ,@(transform-global rest)))]
  [(`(,head ,@tail))
   `(,head ,@(transform-global tail))]
  [('()) '()])

(define/match (compile exp)
  [(`(FUN ,types ,args ,@body))
   `(lambda ,args ,@(map compile body))]
  [(`(APP ,fun ,@args))
   `(,(compile fun) ,@(map compile args))]
  [(`(IF ,test ,then ,else))
   `(if ,(compile test) ,(compile then) ,(compile else))]
  [(`(SET ,var ,val))
   `(set! ,var ,(compile val))]
  [(_) exp])

(define types
  `((+ (number number number))
    (- (number number number))
    (* (number number number))
    (/ (number number number))
    (> (number number bool))
    (< (number number bool))
    (= (number number bool))
    (>= (number number bool))
    (<= (number number bool))
    (,undefined any)
    (read (any))
    (display (any unit))
    (cons (any any cons))
    (car (cons any))
    (cdr (cons any))))

(define (exec exps)
  (let ((exp (apply transform-local (transform-global exps))))
    (typeof exp types)
    (eval (compile exp))))

(define-simple-macro (-module-begin body ...)
  (#%module-begin
   (exec '(body ...))))
