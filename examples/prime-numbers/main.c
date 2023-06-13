int first_divisor(unsigned int a) {
	unsigned int i;

	for (i = 2; i < a; i++) {
		if (a % i == 0) {
			return i;
		}
	}

	return a;
}

void main(void) {
	register int a asm("a0");
	register int is_prime asm("a2");
	register int number asm("a3");

	// a is unconstrained symbolic
	int stack_copy = a;

	if (stack_copy <= 10) {
		if (stack_copy > 1 && first_divisor(stack_copy) == stack_copy) {
			is_prime = 1;
			number = stack_copy;
		} else {
			is_prime = 0;
			number = stack_copy;
		}
	}
}
