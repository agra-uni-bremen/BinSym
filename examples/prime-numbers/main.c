#include <stddef.h>

static void
make_symbolic(volatile void *ptr, size_t size)
{
	__asm__ volatile ("li a7, 96\n"
	                  "mv a0, %0\n"
	                  "mv a1, %1\n"
	                  "ecall\n"
	                  : /* no output operands */
	                  : "r" (ptr), "r" (size)
	                  : "a7", "a0", "a1");
}

static int
first_divisor(unsigned int a)
{
	unsigned int i;

	for (i = 2; i < a; i++) {
		if (a % i == 0) {
			return i;
		}
	}

	return a;
}

void
main(void)
{
	int a;
	register int is_prime asm("a2");
	register int number asm("a3");

	make_symbolic(&a, sizeof(a));
	if (a <= 10) {
		if (a > 1 && first_divisor(a) == a) {
			is_prime = 1;
			number = a;
		} else {
			is_prime = 0;
			number = a;
		}
	}
}
