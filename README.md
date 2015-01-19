# omlogo
Playing around with implementing logo in ocaml.

Loosely based on the UCB logo users manual:
    http://www.cs.berkeley.edu/~bh/usermanual

Current status is a working interpreter with very little builtin functionality. 
Supports ", :, SET, and PRINT. Next steps are defining REPEAT and TO, then the iconic turtle graphics.

NB: currently everything is fully case sensitive.

To build:
    $ ocamlc logo.ml -o logo

To run:
    $ echo "PRINT \"HELLO STOP" | ./logo
    HELLO
