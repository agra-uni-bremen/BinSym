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

int main(void) {
	int a, b, r;

	make_symbolic(&a, sizeof(a));
	make_symbolic(&b, sizeof(b));

	if (a < b) {
		if (a < 5) {
			return 3;
		} else {
			return 2;
		}
	} else {
		return 1;
	}
}
