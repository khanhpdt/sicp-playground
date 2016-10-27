# Basic syntaxes used in SICP

- Variable definition
  
  ```
  (define <name> <value>)
  ```

- Procedure definition
  
  ```
  (define (<name> <formal parameters>)
	  <body>)
  ```

- Varargs (or variadic, or dotted tail) functions
  
  ```
  (define (<name> <normal parameters> . <trailling parameter>) ;; trailing parameter can accept a list of arguments
	  <body>)
  ```

- Conditional expressions
  
  ```
  ;; the predicates are checked in the order they are defined.
  (cond (<pred 1> <exp 1>)
		(<pred 2> <exp 2>)
		...
		(<pred n> <exp n>)
		(else <otherwise expression>)) ;; else is optional
  ```
  
  ```
  (if <predicate> <consequent> <alternative>)
  ```

- Logical operations
  
  ```
  (and <exp 1> ... <exp n>)
  
  (or <exp 1> ... <exp n>)
  
  (not <exp>)
  ```

- Lambda expression
  
  ```
  (lambda (<formal parameters>) 
	  <body>)
  ```
  
- To apply lambda expression without arguments

  ```
  ;; wrap the lambda by parentheses
  ((lambda ()
	  <body>))
  ```

- Binding expressions
  
  ```
  (let ((<var 1> <exp 1>)
		(<var 2> <exp 2>)
		...
		(<var n> <exp n>))
	<body>)
  ```
  
- 

# Native procedures

```
(abs e): absolute value
(cons e1 e2): create pairs
(car e)
(cdr e)
(display)
(gcd e1 e2 ... en): greatest common divisor
(newline)
(min e1 e2 ... en)
(max e1 e2 ... en)
(remainder e1 e2)
(raise e1)
```
