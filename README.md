# omlogo
Playing around with implementing logo in ocaml.

Loosely based on the UCB logo users manual:
    http://www.cs.berkeley.edu/~bh/usermanual

Current status is a working interpreter with very little builtin functionality. 
Supports ", :, SET, and PRINT.

NB: currently everything is fully case sensitive.

To build:
```
	$ ocamlc logo.ml -o logo
```

To run:
```
	$ echo "PRINT \"HELLO STOP" | ./logo
	HELLO
```

## Assignment

The SET builtin takes a place and a value. Usually the place is
specified using the " special form. For example:
```
	$ echo "SET \"X 5
	> PRINT :X
	> STOP" | ./logo
	5
```

## Defining Subroutines

The TO builtin takes a word naming the subroutine, a list, and
consumes words until it reaches the word END. For example:
```
	$ echo "TO SAY [X]
	>   PRINT :X
	>   END
	> SAY \"HELLO
	> STOP" | ./logo
	HELLO
```
	
## Iteration

The REPEAT builtin takes an expression that evaluates to a numeric
count, and evaluates subsequent words until the word END that many times.. 
```
	$ echo "REPEAT 5
	>   PRINT \"HELLO
	>   END
	> STOP" | ./logo
	HELLO
	HELLO
	HELLO
	HELLO
	HELLO
```

## Conditionals

The IF/THEN/ELSE/END construct takes a condition expression that evaluates to 0 or
non-zero. If the condition is non-zero, the words between the
condition and the ELSE (or END if ELSE is omitted) are evaluated. If
the condition is zero, the words between ELSE and END are evaluated.
```
	$ echo "SET \"X 5
	> IF X THEN
	>   PRINT \"HELLO
	> ELSE
	>   PRINT \"GOODBYE
	> END" | ./logo
	HELLO
```
