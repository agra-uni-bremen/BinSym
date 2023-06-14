#!/bin/sh
set -x
riscv-none-elf-gcc -march=rv32i -mabi=ilp32 -nostdlib -o main main.S
