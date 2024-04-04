# BinSym

Symbolic execution of [RISC-V] binary code based on formal instruction semantics.

## About

BinSym is a program analysis tool which enables symbolic execution of binary code.
The majority of prior work on binary program analysis lifts/transforms binary code
to an Intermediate Representation (IR) and then analysis this intermediate format.
BinSym, on the other hand, operates directly on the binary-level at eliminates
the need to perform binary lifting. This enables BinSym to capture and reason
about low-level interactions (e.g. with the architectural state). Furthermore,
through the utilization of formal instruction semantics, BinSym is more faithful
to the ISA specification and eliminates the possibilities of errors and inaccuracies
which may occur during the lifting step in prior work.

The implementation of BinSym is based on our prior work on [LibRISCV]. Specifically,
BinSym provides actual symbolic semantics for the abstract instruction semantics
specified in LibRISCV. Or, in other words, BinSym is a symbolic free monad interpreter
for LibRISCV.

## Installation

BinSym has been developed for GHC 9.4.8 (newer versions may work too). To install
BinSym run the following commands:

	$ git clone https://github.com/agra-uni-bremen/binsym
	$ cabal install

This install a `formal-symex` binary into your path. This binary can be used for
symbolic execution of RV32I machine code. Within this machine code, unconstrained
symbolic values can be declared based on which the code is then symbolically explored.
In order to declare a symbolic value, the following code can be used:

```C
void
make_symbolic(void *ptr, size_t size)
{
	__asm__ volatile ("li a7, 96\n"
	                  "mv a0, %0\n"
	                  "mv a1, %1\n"
	                  "ecall\n"
	                  : /* no output operands */
	                  : "r" (ptr), "r" (size)
	                  : "a7", "a0", "a1");
}
```

BinSym executes the code until it finds the first invalid instruction, therefore
in order to terminate an execution use something along the lines of `.word 0xffff`
in your startup assembly file.

[RISC-V]: https://riscv.org/
[LibRISCV]: https://github.com/agra-uni-bremen/libriscv