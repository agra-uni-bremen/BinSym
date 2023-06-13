#!/bin/sh
set -x
riscv-none-elf-gcc -march=rv32i -mabi=ilp32 -nostartfiles -o main start.S main.c
