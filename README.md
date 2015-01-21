# omlogo

Playing around with implementing logo in ocaml.

(Very) Loosely based on the UCB logo users manual:
    http://www.cs.berkeley.edu/~bh/usermanual

Current status is a working interpreter with very little builtin functionality. 
Supports ", :, SET, and PRINT, TO, REPEAT, IF, IFELSE, and basic
graphics commands FORWARD, TURN, PENUP, PENDOWN

NB: currently everything is fully case sensitive.

Requires ocaml >= 4.0.2 with the Graphics module (not the default on
OS X systems). My best experience has been to install a system ocaml
compiler and opam, then use opam to install a new compiler:
```
	$ opam switch 4.0.2
	$ eval `opam config env`
```

To build omlogo:
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

The IF builtin takes a condition expression that evaluates to TRUE or
FALSE, and one list. If the condition is TRUE then the list is
evaluated, otherwise nothing is done.

The IFELSE builtin is similar but takes an additional list argument
and evaluates it if the condition is FALSE.

```
	$ echo "SET \"X 5
	> IFELSE :X [PRINT \"HELLO]
	>   [PRINT \"GOODBYE]
	>STOP" | ./logo
	HELLO
```

## Arithmetic

Basic arithmetic on integers is supported with SUM, DIFFERENCE,
PRODUCT, QUOTIENT, and REMAINDER. Infix operators are not supported.

## Graphics

We don't currently draw the turtle in the display. The turtle is
initially positioned in the center of the 600x600 pixel graph window
oriented facing the right. See fractal.logo for a graphical
example. To run use:

```
	$ cat fractal.logo /dev/stdin | ./logo
	DRAWFRAC 200
```

Resizing the graphics window will likely cause strange things to happen.
