#!/bin/sh
riscv32-unknown-elf-gcc -march=rv32i -mabi=ilp32 -nostdlib -o main main.S
