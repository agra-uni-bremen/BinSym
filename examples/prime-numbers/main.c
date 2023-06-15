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
	register int _a asm("a0");
	register int is_prime asm("a2");
	register int number asm("a3");

	// a is unconstrained symbolic
	int a = _a;

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
