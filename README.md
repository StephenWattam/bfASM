# bfASM
An x86\_64 ASM brainfuck interpreter.

## Compilation
Simply run make to build the `bfi` binary:

    $ make

## Use
Help text is printed to the screen if no arguments are given:

    $ ./bfi 
    USAGE:
      bfi [EOF] FILE

    EOF can be:
      z : Set EOF == 0.
      n : Set EOF == -1.

