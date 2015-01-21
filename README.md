# omlogo
Playing around with implementing logo in ocaml.

Loosely based on the UCB logo users manual:
    http://www.cs.berkeley.edu/~bh/usermanual

Current status is a working interpreter with very little builtin functionality. 
Supports ", :, SET, and PRINT, TO, REPEAT, IF/THEN/ELSE/END, and basic
graphics commands FORWARD, TURN, PENUP, PENDOWN

NB: currently everything is fully case sensitive.

ERRATA: Constructs requiring an END word don't nest. This is due to
the stupid stream of tokens parsing we're using/the fact that we don't
know how many words to consume for each instruction until evaluation
time. We could give these constructs (TO, REPEAT, IF/THEN/ELSE)
special syntax rules...pondering.

To build:
```
	$ make
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

The TO builtin takes a word naming the subroutine, a quoted list of
parameter names, and consumes words until it reaches the word END.

Note: TO is a special form that does not evaluate its arguments, the
procedure name and parameters must be syntactically a word and a
quoted list of words.

For example:
```
	$ echo "TO SAY [X]
	>   PRINT :X
	>   END
	> SAY \"HELLO
	> STOP" | ./logo
	HELLO
```
	
## Iteration

The REPEAT builtin takes a numeric count, and a list and evaluates the
list count times. 

```
	$ echo "REPEAT 5 [PRINT \"HELLO] STOP" | ./logo
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
	> IF :X THEN
	>   PRINT \"HELLO
	> ELSE
	>   PRINT \"GOODBYE
	> END" | ./logo
	HELLO
```
